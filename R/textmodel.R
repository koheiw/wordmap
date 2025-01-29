#' A model for multinomial feature extraction and document classification
#'
#' Wordmap is a model for multinomial feature extraction and document
#' classification. Its naive Bayesian algorithm allows users to train the model
#' on a large corpus with noisy labels given by document meta-data or keyword
#' matching.
#' @param x a dfm or fcm created by [quanteda::dfm()]
#' @param y a dfm or a sparse matrix that record class membership of the
#'   documents. It can be created applying [quanteda::dfm_lookup()] to `x`.
#' @param label if "max", uses only labels for the maximum value in each row of
#'   `y`.
#' @param drop_label if `TRUE`, drops empty columns of `y` and ignore their
#'   labels.
#' @param smooth the amount of smoothing in computing coefficients.
#'   When `smooth = 0.01`, 1% of the mean frequency of words in each class
#'   is added to smooth likelihood ratios.
#' @param boolean if `TRUE`, only consider presence or absence of features in
#'   each document to limit the impact of words repeated in few documents.
#' @param residual if `TRUE`, a residual class is added to `y`. It is named
#'   "other" but can be changed via `base::options(wordmap_residual_name)`.
#' @param entropy the scheme to compute the entropy to
#'   regularize likelihood ratios. The entropy of features are computed over
#'   labels if `global` or over documents with the same labels if `local`. Local
#'   entropy is averaged if `average`. See the details.
#' @param verbose if `TRUE`, shows progress of training.
#' @param ... additional arguments passed to internal functions.
#' @details Wordmap learns association between words in `x` and classes in `y`
#'   based on likelihood ratios. The large
#'   likelihood ratios tend to concentrate to a small number of features but the
#'   entropy of their frequencies over labels or documents helps to disperse the
#'   distribution.
#'
#'   A residual class is created internally by adding a new column to `y`.
#'   The column is given 1 if the other values in the same row are all zero
#'    (i.e. `rowSums(y) == 0`); otherwise 0. It is useful when users cannot create
#'    an exhaustive dictionary that covers all the categories.
#'
#' @importFrom quanteda is.dfm as.dfm dfm_trim nfeat
#' @references Watanabe, Kohei (2018). "Newsmap: semi-supervised approach to
#'   geographical news classification". doi.org/10.1080/21670811.2017.1293487,
#'    *Digital Journalism*.
#' @references Watanabe, Kohei & Zhou, Yuan (2020). "Theory-Driven Analysis of
#'   Large Corpora: Semisupervised Topic Classification of the UN Speeches".
#'   doi:10.1177/0894439320907027. *Social Science Computer Review*.
#' @returns Returns a fitted textmodel_wordmap object with the following
#'   elements: \item{model}{a matrix that records the association between
#'   classes and features.}
#'   \item{data}{the original input of `x`.}
#'   \item{feature}{the feature set in `x`}
#'   \item{class}{the class labels in `y`.}
#'   \item{concatenator}{the concatenator in `x`.}
#'   \item{entropy}{the scheme to compute entropy weights.}
#'   \item{boolean}{the use of the Boolean transformation of `x`.}
#'   \item{call}{the command used to execute the function.}
#'   \item{version}{the version of the wordmap package.}
#' @export
#' @examples
#' require(quanteda)
#'
#' # split into sentences
#' corp <- corpus_reshape(data_corpus_ungd2017)
#'
#' # tokenize
#' toks <- tokens(corp, remove_punct = TRUE) %>%
#'    tokens_remove(stopwords("en"))
#'
#' # apply seed dictionary
#' toks_dict <- tokens_lookup(toks, data_dictionary_topic)
#'
#' # form dfm
#' dfmt_feat <- dfm(toks)
#' dfmt_dict <- dfm(toks_dict)
#'
#' # fit wordmap model
#' map <- textmodel_wordmap(dfmt_feat, dfmt_dict)
#' coef(map)
#' predict(map)
#'
#' @export
textmodel_wordmap <- function(x, y, label = c("all", "max"), smooth = 0.01,
                              boolean = FALSE, drop_label = TRUE,
                              entropy = c("none", "global", "local", "average"),
                              residual = FALSE,
                              verbose = quanteda_options('verbose'),
                              ...) {
    UseMethod("textmodel_wordmap")
}

#' @noRd
#' @export
#' @importFrom quanteda check_double check_logical
textmodel_wordmap.dfm <- function(x, y, label = c("all", "max"), smooth = 0.01,
                                  boolean = FALSE, drop_label = TRUE,
                                  entropy = c("none", "global", "local", "average"),
                                  residual = FALSE,
                                  verbose = quanteda_options('verbose'), ...,
                                  old = FALSE) {

    unused_dots(...)
    entropy <- match.arg(entropy)
    label <- match.arg(label)
    smooth <- check_double(smooth, min = 0)
    boolean <- check_logical(boolean)
    drop_label <- check_logical(drop_label)
    residual <- check_logical(residual)

    if (label == "max") {
        y <- as(as(as(y, "dMatrix"), "generalMatrix"), "TsparseMatrix")
        s <- sapply(split(y@x, y@i + 1L), max)
        y@x[y@x < s[y@i + 1L]] <- 0L
    }

    if (nrow(x) != nrow(y))
        stop("x and y must have the same number of rows")
    if (!is.null(rownames(y)) && !identical(rownames(x), rownames(y)))
        warning("x and y have different rownames")

    w <- dfm_trim(x, min_termfreq = 1)
    if (boolean)
        w <- dfm_weight(w, "boolean")
    y <- as.dfm(y)
    if (drop_label)
        y <- dfm_trim(y, min_termfreq = 1)

    if (residual) {
        label <- getOption("wordmap_residual_name", "other")
        res <- Matrix::sparseMatrix(i = seq_len(nrow(y)),
                                    j = rep(1, nrow(y)),
                                    x = rowSums(y) == 0,
                                    dim = c(nrow(y), 1),
                                    dimnames = list(rownames(y), label))
        y <- cbind(y, as.dfm(res))
    }

    if (!nfeat(w))
        stop("x must have at least one non-zero feature")
    if (!nfeat(y))
        stop("y must have at least one non-zero feature")

    model <- matrix(rep(0, ncol(w) * ncol(y)), ncol = ncol(w), nrow = ncol(y),
                    dimnames = list(colnames(y), colnames(w)))

    if (verbose)
        cat("Fitting textmodel_wordmap...\n")

    if (entropy == "global") {
        e <- get_entropy(w, nrow(w)) # e = 1.0 for uniform distribution
        weight <- matrix(rep(e, each = ncol(y)), ncol = ncol(w), nrow = ncol(y),
                         dimnames = list(colnames(y), colnames(w)))
    } else {
        weight <- matrix(rep(1, ncol(w) * ncol(y)), ncol = ncol(w), nrow = ncol(y),
                         dimnames = list(colnames(y), colnames(w)))
    }

    m <- colSums(w)
    for (key in sort(featnames(y))) {
        if (verbose) cat(sprintf('  label = "%s"\n', key))
        z <- w[as.logical(y[,key] > 0),]
        s <- colSums(z)
        if (old) {
            v0 <- m - s + smooth
            v1 <- s + smooth
        } else {
            if (smooth >= 1.0)
                warning("The value of smooth became fractional in wordmap v0.92")
            a <- mean(s)
            v0 <- m - s + (a * smooth) + (1 / length(s))
            v1 <- s + (a * smooth) + (1 / length(s))
        }
        model[key,] <- log(v1 / sum(v1)) - log(v0 / sum(v0)) # log-likelihood ratio

        if (entropy %in% c("local", "average")) {
            if (nrow(z) > 1) {
                weight[key,] <- get_entropy(z, nrow(z)) # e = 1.0 for uniform distribution
            } else {
                if (entropy == "local") {
                    weight[key,] <- 0
                } else {
                    weight[key,] <- NA
                }
            }
        }
    }

    if (entropy == "average") {
        e <- colMeans(weight, na.rm = TRUE)
        weight <- matrix(rep(e, each = ncol(y)), ncol = ncol(w), nrow = ncol(y),
                         dimnames = list(colnames(y), colnames(w)))
    }

    result <- list(model = model,
                   data = x,
                   weight = NULL,
                   feature = colnames(model),
                   class = rownames(model),
                   concatenator = meta(x, field = "concatenator", type = "object"),
                   entropy = entropy,
                   boolean = boolean,
                   call = try(match.call(sys.function(-1), call = sys.call(-1)), silent = TRUE),
                   version = utils::packageVersion("wordmap"))
    if (entropy != "none")
        result$weight <- weight
    class(result) <- "textmodel_wordmap"
    return(result)
}

#' @noRd
#' @method summary textmodel_wordmap
#' @export
summary.textmodel_wordmap <- function(object, n = 10, ...) {
    result <- list(
        "call" = object$call,
        "labels" = rownames(object$model)
    )
    if (!is.null(object$data))
        result$data.dimension <- dim(object$data)
    as.summary.textmodel(result)
}

#' Extract coefficients from a Wordmap model
#'
#' `coef()` extracts top `n` features with largest coefficients for each class.
#' @param object a model fitted by [textmodel_wordmap()].
#' @param n the number of coefficients to extract.
#' @param select returns the coefficients for the selected class; specify by the
#'   names of rows in `object$model`.
#' @param ... not used.
#' @returns Returns a list of named numeric vectors sorted in descending order.
#' @method coef textmodel_wordmap
#' @import Matrix
#' @importFrom stats coef
#' @export
coef.textmodel_wordmap <- function(object, n = 10, select = NULL, ...) {

    n <- check_integer(n, min = 0)
    select <- check_character(select, min_len = 1, max_len = nrow(object$model),
                              strict = TRUE, allow_null = TRUE)

    if (is.null(select)) {
        j <- rep(TRUE, nrow(object$model))
    } else {
        if (any(!select %in% rownames(object$model)))
            stop("Selected class must be in the model", call. = FALSE)
        j <- rownames(object$model) %in% select
    }

    if (is.null(object$weight)){
        model <- object$model
    } else {
        model <- object$model * object$weight
    }

    model <- as(as(as(model, "dMatrix"), "generalMatrix"), "TsparseMatrix")
    model <- model[j,, drop = FALSE]
    temp <- model@x
    names(temp) <- colnames(object$model)[model@j + 1L]
    result <- split(temp, factor(model@i + 1L, levels = seq_len(nrow(model)),
                                 labels = rownames(model)))
    result <- lapply(result, function(x) head(sort(x, decreasing = TRUE), n))
    return(result)
}

#' @rdname coef.textmodel_wordmap
#' @method coefficients textmodel_wordmap
#' @importFrom stats coefficients
#' @export
coefficients.textmodel_wordmap <- function(object, n = 10, select = NULL, ...) {
    UseMethod("coef")
}

#' Create lexicon from a Wordmap model
#'
#' `as.list()` returns features with the largest coefficients as a list of
#' character vector. `as.dictionary()` returns a [quanteda::dictionary] object
#' that can be use for dictionary analysis.
#' @param x a model fitted by [textmodel_wordmap()].
#' @export
#' @param separator the character in between multi-word dictionary values. If
#'   `NULL`, `x$concatenator` will be used.
#' @return Returns a list or a [quanteda::dictionary] object.
#' @method as.dictionary textmodel_wordmap
as.dictionary.textmodel_wordmap <- function(x, separator = NULL, ...) {
    if (is.null(separator))
        separator <- x$concatenator
    dictionary(lapply(coef(x, ...), names), separator = separator)
}

#' @rdname as.dictionary.textmodel_wordmap
#' @export
#' @method as.list textmodel_wordmap
#' @param ... passed to [coef.textmodel_wordmap]
as.list.textmodel_wordmap <- function(x, ...) {
    lapply(coef(x, ...), names)
}

#' @importFrom quanteda dfm_subset
group_topics <- function(x, y) {
    result <- matrix(NA, nrow = nfeat(y), ncol = nfeat(x),
                     dimnames = list(featnames(y), featnames(x)))
    for (i in seq_len(nfeat(y))) {
        result[i, ] <- colSums(dfm_subset(x, rowSums(y[ ,i]) > 0))
    }
    return(as.dfm(result))
}

#' @importFrom quanteda dfm_weight
get_entropy <- function(x, base = 2) {

    x <- t(x)
    x <- dfm_weight(x, "prop")
    x <- as(as(as(x, "dMatrix"), "generalMatrix"), "TsparseMatrix")
    result <- unlist(lapply(split(x@x, factor(x@i + 1L, levels = seq_len(nrow(x)))),
                       function(y) sum(y * log(y, base)) * -1), use.names = FALSE)
    names(result) <- rownames(x)
    return(result)
}
