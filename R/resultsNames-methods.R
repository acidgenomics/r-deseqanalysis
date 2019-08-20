#' @name resultsNames
#' @inherit bioverbs::resultsNames
#' @note Updated 2019-08-20.
#'
#' @inheritParams acidroxygen::params
#' @inheritParams params
#' @param ... Additional arguments.
#'
#' @examples
#' data(deseq)
#' resultsNames(deseq)
NULL



#' @rdname resultsNames
#' @name resultsNames
#' @importFrom bioverbs resultsNames
#' @usage resultsNames(object, ...)
#' @export
NULL



## Updated 2019-07-23.
`resultsNames,DESeqDataSet` <-  # nolint
    function(object) {
        DESeq2::resultsNames(object)
    }



#' @rdname resultsNames
#' @export
setMethod(
    f = "resultsNames",
    signature = signature("DESeqDataSet"),
    definition = `resultsNames,DESeqDataSet`
)



## Updated 2019-07-23.
`resultsNames,DESeqAnalysis` <-  # nolint
    function(object) {
        names(object@results)
    }



#' @rdname resultsNames
#' @export
setMethod(
    f = "resultsNames",
    signature = signature("DESeqAnalysis"),
    definition = `resultsNames,DESeqAnalysis`
)
