# FIXME Export this in bioverbs.



#' Results names
#'
#' @name resultsNames
#' @inheritParams params
#'
#' @seealso [DESeq2::resultsNames()].
NULL



#' @rdname resultsNames
#' @export
setGeneric(
    name = "resultsNames",
    def = function(object, ...) {
        standardGeneric("resultsNames")
    }
)



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
