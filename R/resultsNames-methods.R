#' @name resultsNames
#' @inherit bioverbs::resultsNames
#' @inheritParams basejump::params
#' @inheritParams params
NULL



#' @importFrom bioverbs resultsNames
#' @aliases NULL
#' @export
bioverbs::resultsNames



resultsNames.DESeqDataSet <- function(object) {
    DESeq2::resultsNames(object)
}



#' @rdname resultsNames
#' @export
setMethod(
    f = "resultsNames",
    signature = signature("DESeqDataSet"),
    definition = resultsNames.DESeqDataSet
)



resultsNames.DESeqAnalysis <- function(object) {
    names(object@results)
}



#' @rdname resultsNames
#' @export
setMethod(
    f = "resultsNames",
    signature = signature("DESeqAnalysis"),
    definition = resultsNames.DESeqAnalysis
)
