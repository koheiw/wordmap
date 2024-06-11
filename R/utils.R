#' @keywords internal
#' @export
#' @method as.list textmodel_wordmap
#' @param ... passed to [coef.textmodel_wordmap]
as.list.textmodel_wordmap <- function(x, ...) {
    lapply(coef(x, ...), names)
}

#' @export
#' @method as.dictionary textmodel_wordmap
as.dictionary.textmodel_wordmap <- function(x, ...) {
    dictionary(lapply(coef(x, ...), names), separator = x$concatenator)
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
