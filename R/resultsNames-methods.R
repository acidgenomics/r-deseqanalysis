#' @name resultsNames
#' @inherit bioverbs::resultsNames
#'
#' @inheritParams basejump::params
#' @inheritParams params
#' @param ... Additional arguments.
#'
#' @examples
#' data(deseq)
#' resultsNames(deseq)
NULL



resultsNames.DESeqDataSet <-  # nolint
    function(object) {
        DESeq2::resultsNames(object)
    }



#' @rdname resultsNames
#' @export
setMethod(
    f = "resultsNames",
    signature = signature("DESeqDataSet"),
    definition = resultsNames.DESeqDataSet
)



resultsNames.DESeqAnalysis <-  # nolint
    function(object) {
        names(object@results)
    }



#' @rdname resultsNames
#' @export
setMethod(
    f = "resultsNames",
    signature = signature("DESeqAnalysis"),
    definition = resultsNames.DESeqAnalysis
)
