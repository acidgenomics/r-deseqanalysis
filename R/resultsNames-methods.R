#' @name resultsNames
#' @export
#' @importFrom bioverbs resultsNames
#' @inherit bioverbs::resultsNames
#' @inheritParams params
NULL



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
