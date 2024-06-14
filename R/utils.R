#' Evaluate classification accuracy in precision and recall
#'
#' `accuracy()` counts a data.frame that contains number true positive
#' (tp), false positive (fp), true negative (tn) and false negative (fn) cases
#' for each predicted class and calculates precision, recall and F1 score
#' based on these counts.
#' `summary()` calculates micro-average precision (p) and recall (r) and
#' macro-average precision (P) and recall (R) based on the output of
#' `accuracy()`.
#' @param x vector of predicted classes.
#' @param y vector of true classes.
#' @export
#' @examples
#' class_pred <- c('US', 'GB', 'US', 'CN', 'JP', 'FR', 'CN') # prediction
#' class_true <- c('US', 'FR', 'US', 'CN', 'KP', 'EG', 'US') # true class
#' acc <- accuracy(class_pred, class_true)
#' print(acc)
#' summary(acc)
accuracy <- function(x, y) {

    temp <- data.frame(test = x, true = y)
    temp <- temp[!is.na(temp$true),,drop = FALSE] # remove unknown in true class

    label <- sort(unique(temp$true))
    result <- data.frame()
    for(l in label){
        tp <- sum(temp$true == l & temp$test == l, na.rm = TRUE)
        fp <- sum(temp$true != l & temp$test == l, na.rm = TRUE)
        tn <- sum(temp$true != l & temp$test != l, na.rm = TRUE)
        fn <- sum(temp$true == l & temp$test != l, na.rm = TRUE)
        precision <- tp / (tp + fp)
        recall <- tp / (tp + fn)
        f1 <- (2 * precision * recall) / (precision + recall)
        result <- rbind(result, data.frame(tp, fp, tn, fn, precision, recall, f1))
    }
    class(result) <- c('textmodel_wordmap_accuracy', class(result))
    rownames(result) <- label
    return(result)
}

#' @rdname accuracy
#' @param object output of `accuracy()`.
#' @param ... not used.
#' @method summary textmodel_wordmap_accuracy
#' @export
summary.textmodel_wordmap_accuracy <- function(object, ...) {

    #Micro-average of precision = (TP1+TP2)/(TP1+TP2+FP1+FP2)
    p <- sum(object[,'tp'], na.rm = TRUE) / sum(object[,c('tp', 'fp')])
    #Micro-average of recall = (TP1+TP2)/(TP1+TP2+FN1+FN2)
    r <- sum(object[,'tp'], na.rm = TRUE) / sum(object[,c('tp', 'fn')])
    #Macro-average precision = (P1+P2)/2
    P <- sum(object[,'precision'], na.rm = TRUE) / nrow(object)
    #Macro-average recall = (R1+R2)/2
    R <- sum(object[,'recall'], na.rm = TRUE) / nrow(object)

    result <- c(p = p, r = r, P = P, R = R)
    return(result)
}

#' Compute Average Feature Entropy (AFE)
#'
#' `afe()` computes Average Feature Entropy (AFE), which measures randomness of
#' occurrences of features in labelled documents (Watanabe & Zhou, 2020). In
#' creating seed dictionaries, AFE can be used to avoid adding seed words that would
#' decrease classification accuracy.
#' @param x a dfm for features.
#' @param y a dfm for labels.
#' @param smooth a numeric value for smoothing to include all the features.
#' @export
#' @references Watanabe, Kohei & Zhou, Yuan (2020). "Theory-Driven Analysis of
#'   Large Corpora: Semisupervised Topic Classification of the UN Speeches".
#'   doi:10.1177/0894439320907027. *Social Science Computer Review*.
afe <- function(x, y, smooth = 1) {
    if (!is.dfm(x) || !is.dfm(y))
        stop('x and y have to be dfm')
    e <- get_entropy(group_topics(x, y) + smooth)
    if (is.data.frame(e))
        e <- e$entropy
    return(mean(e))
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
