#' Plot coefficients of words
#' @param x a fitted textmodel_wordmap object.
#' @param highlighted [quanteda::pattern] to select words to highlight. If a
#'   [quanteda::dictionary] is passed, words in the top-level categories are
#'   highlighted in different colors.
#' @param max_highlighted the maximum number of words to highlight. When
#'   `highlighted = NULL`, words to highlight are randomly selected
#'   proportionally to `coef ^ 2`.
#' @param max_words the maximum number of words to plot. Words are randomly
#'   sampled to keep the number below the limit.
#' @param ... passed to underlying functions. See the Details.
#' @details Users can customize the plots through `...`, which is
#'   passed to [ggplot2::geom_text()] and [ggrepel::geom_text_repel()]. The
#'   colors are specified internally but users can override the settings by appending
#'   [ggplot2::scale_colour_manual()] or [ggplot2::scale_colour_brewer()]. The
#'   legend title can also be modified using [ggplot2::labs()].
#' @importFrom ggrepel geom_text_repel
#' @export
textplot_terms <- function(x, highlighted = NULL,
                           max_highlighted = 50, max_words = 1000, ...) {
    UseMethod("textplot_terms")
}

#' @method textplot_terms textmodel_wordmap
#' @import ggplot2 ggrepel stringi
#' @importFrom quanteda is.dictionary meta check_integer
#' @export
textplot_terms.textmodel_wordmap <- function(x, highlighted = NULL,
                                         max_highlighted = 50, max_words = 1000, ...) {

    max_words <- check_integer(max_words, min = 1)
    max_highlighted <- check_integer(max_highlighted, min = 0)

    x$frequency <- x$frequency[names(x$beta)] # fix for < v1.1.4
    x$frequency[is.na(x$frequency)] <- 0

    beta <- freq <- word <- NULL
    freq <- featfreq(x$data)
    # TODO: save class names in the model
    mat <- x$model[!rownames(x$model) %in% getOption("wordmap_residual_name", "other"),]
    temp <- data.frame(word = colnames(mat)[col(mat)],
                       group = rownames(mat)[row(mat)],
                       beta = as.vector(mat),
                       freq = freq[col(mat)])

    temp <- subset(temp, freq > 0)
    temp$freq <- log(temp$freq)
    temp$id <- seq_len(nrow(temp))
    temp$match <- rep(TRUE, nrow(temp))

    if (is.null(highlighted)) {
        key <- NULL
    } else {
        if (is.dictionary(highlighted)) {
            separator <- meta(highlighted, field = "separator", type = "object")
            valuetype <- meta(highlighted, field = "valuetype", type = "object")
        } else {
            highlighted <- unlist(highlighted, use.names = FALSE)
            valuetype <- "glob"
        }
        if (is.null(x$concatenator)) {
            concatenator <- "_" # for old object
        } else {
            concatenator <- x$concatenator
        }
        ids <- quanteda::object2id(
            highlighted,
            types = temp$word,
            valuetype = valuetype,
            case_insensitive = TRUE,
            concatenator = concatenator
        )

        # flag nested patterns (see quanteda::dfm_lookup)
        if (length(ids)) {
            m <- factor(names(ids), levels = unique(names(ids)))
            dup <- unlist(lapply(split(ids, m), duplicated), use.names = FALSE)
        } else {
            dup <- logical()
        }
        ids <- ids[lengths(ids) == 1 & !dup] # drop phrasal and nested patterns
        id <- unlist(ids)

        if (!is.null(id)) {
            temp$match <- temp$id %in% id
        }
    }

    temp$p <- as.numeric(temp$match) * temp$beta ^ 2
    if (all(temp$p == 0)) {
        l <- rep(FALSE, length(temp$id))
    } else {
        l <- temp$id %in% sample(temp$id, min(sum(temp$p > 0), max_highlighted),
                                 prob = temp$p)
    }
    temp_hi <- temp[l,]
    temp_lo <- temp[!l,]

    group <- NULL # for cran check
    temp_lo <- head(temp_lo[sample(seq_len(nrow(temp_lo))),], max_words)
    gg <- ggplot(data = temp_lo, aes(x = beta, y = freq, label = word)) +
        geom_text(colour = "grey70", alpha = 0.7, ...) +
        labs(x = "Coefficient", y = "Frequency (log)") +
        theme_bw() +
        theme(panel.grid= element_blank(),
              axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
              axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
        geom_text_repel(data = temp_hi, aes(x = beta, y = freq, label = word, colour = group),
                        segment.size = 0.25, show.legend = FALSE, ...) +
        geom_point(data = temp_hi, aes(x = beta, y = freq, shape = group, colour = group),
                   cex = 1) +
        scale_colour_brewer(palette = "Set1", drop = FALSE) +
        scale_shape_discrete(drop = FALSE) +
        labs(colour = "Class", shape = "Class")

    return(gg)
}
