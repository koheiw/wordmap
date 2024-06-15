#' Semi-supervised model for multinomial document classification
#'
#' Wordmap is a semi-supervised Bayesian model for multinomial document
#' classification. Wordmap models are usually fitted using labels given by
#' dictionary analysis or document meta-data.
#' @param x a dfm or fcm created by [quanteda::dfm()]
#' @param y a dfm or a sparse matrix that record class membership of the
#'   documents. It can be created applying [quanteda::dfm_lookup()] to `x`.
#' @param label if "max", uses only labels for the maximum value in each row of
#'   `y`.
#' @param drop_label if `TRUE`, drops empty columns of `y` and ignore their
#'   labels.
#' @param smooth a value added to the frequency of words to smooth likelihood
#'   ratios.
#' @param boolean if `TRUE`, only consider presence or absence of features in
#'   each document to limit the impact of words repeated in few documents.
#' @param verbose if `TRUE`, shows progress of training.
#' @param entropy \[experimental\] the scheme to compute the entropy to
#'   regularize likelihood ratios. The entropy of features are computed over
#'   labels if `global` or over documents with the same labels if `local`. Local
#'   entropy is averaged if `average`. See the details.
#' @param ... additional arguments passed to internal functions.
#' @details Wordmap learns association between words and classes as likelihood
#'   ratios based on the features in `x` and the labels in `y`. The large
#'   likelihood ratios tend to concentrate to a small number of features but the
#'   entropy of their frequencies over labels or documents helps to disperse the
#'   distribution.
#'
#' @importFrom quanteda is.dfm as.dfm dfm_trim nfeat
#' @references Watanabe, Kohei (2018). "Newsmap: semi-supervised approach to
#'   geographical news classification". doi.org/10.1080/21670811.2017.1293487,
#'    *Digital Journalism*.
#' @references Watanabe, Kohei & Zhou, Yuan (2020). "Theory-Driven Analysis of
#'   Large Corpora: Semisupervised Topic Classification of the UN Speeches".
#'   doi:10.1177/0894439320907027. *Social Science Computer Review*.
#' @returns Returns a fitted textmodel_wordmap object with the following elements:
#'   \item{model}{A matrix that records the association between classes and features.}
#'   \item{data}{The original input of `x`.}
#'   \item{feature}{The feature set in the model.}
#'   \item{concatenator}{The concatenator in `x`.}
#'   \item{entropy}{The type of entorpy weights used.}
#'   \item{boolean}{The use of Booelan transformation of `x`.}
#'   \item{call}{The command used to run the function.}
#'   \item{version}{The version of the wordmap package.}
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
textmodel_wordmap <- function(x, y, label = c("all", "max"), smooth = 1.0,
                              boolean = FALSE, drop_label = TRUE,
                              verbose = quanteda_options('verbose'),
                              entropy = c("none", "global", "local", "average"), ...) {
    UseMethod("textmodel_wordmap")
}

#' @noRd
#' @export
#' @importFrom quanteda check_double check_logical
textmodel_wordmap.dfm <- function(x, y, label = c("all", "max"), smooth = 1.0,
                                  boolean = FALSE, drop_label = TRUE,
                                  verbose = quanteda_options('verbose'),
                                  entropy = c("none", "global", "local", "average"), ...) {

    unused_dots(...)
    entropy <- match.arg(entropy)
    label <- match.arg(label)
    smooth <- check_double(smooth, min = 0)
    boolean <- check_logical(boolean)
    drop_label <- check_logical(drop_label)

    if (label == "max") {
        y <- as(as(as(y, "dMatrix"), "generalMatrix"), "TsparseMatrix")
        s <- sapply(split(y@x, y@i + 1L), max)
        y@x[y@x < s[y@i + 1L]] <- 0L
    }

    w <- dfm_trim(x, min_termfreq = 1)
    if (boolean)
        w <- dfm_weight(w, "boolean")
    y <- as.dfm(y)
    if (drop_label)
        y <- dfm_trim(y, min_termfreq = 1)

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
        v0 <- m - s + smooth
        v1 <- s + smooth
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
#' `coef()` returns coefficients of features as a list of numeric vector; the
#' numeric vectors are soted in decending order by the sizes of coefficients.
#' @param object a model fitted by [textmodel_wordmap()].
#' @param n the number of coefficients to extract.
#' @param select returns the coefficients for the selected class; specify by the
#'   names of rows in `object$model`.
#' @param ... not used.
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
#' `as.list()` returns features with the largest coefficientsa as a list of
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
