#' @name results
#' @export
#' @importFrom bioverbs results
#' @inherit bioverbs::results
#' @inheritParams params
NULL



results.DESeqDataSet <- function(object, ...) {
    DESeq2::results(object, ...)
}



#' @rdname results
#' @export
setMethod(
    f = "results",
    signature = signature("DESeqDataSet"),
    definition = results.DESeqDataSet
)



results.DESeqAnalysis <- function(object, results, lfcShrink = TRUE) {
    .matchResults(object = object, results = results, lfcShrink = lfcShrink)
}



#' @rdname results
#' @export
setMethod(
    f = "results",
    signature = signature("DESeqAnalysis"),
    definition = results.DESeqAnalysis
)
