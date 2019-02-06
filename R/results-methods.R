# FIXME Export this in bioverbs.



#' Results
#'
#' @name results
#' @inheritParams params
#'
#' @seealso [DESeq2::results()].
NULL



#' @rdname results
#' @export
setGeneric(
    name = "results",
    def = function(object, ...) {
        standardGeneric("results")
    }
)



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
