#' @keywords internal
#' @export
#' @method as.list textmodel_wordmap
#' @param ... passed to [coef.textmodel_wordmap]
as.list.textmodel_wordmap <- function(x, ...) {
    lapply(coef(x, ...), names)
}

#' @export
#' @param separator the character in between multi-word dictionary values. If
#'   `NULL`, `x$concatenator` will be used.
#' @method as.dictionary textmodel_wordmap
as.dictionary.textmodel_wordmap <- function(x, separator = NULL, ...) {
    if (is.null(separator))
        separator <- x$concatenator
    dictionary(lapply(coef(x, ...), names), separator = separator)
}

#' @export
#' @method print textmodel_wordmap
print.textmodel_wordmap <- function(x, ...) {
    cat("\nCall:\n")
    print(x$call)
    cat("\n")
}

unused_dots <- function(...) {
    arg <- names(list(...))
    if (length(arg) == 1) {
        warning(arg[1], " argument is not used.\n", call. = FALSE)
    } else if (length(arg) > 1) {
        warning(paste0(arg, collapse = ", "), " arguments are not used.\n", call. = FALSE)
    }
}
