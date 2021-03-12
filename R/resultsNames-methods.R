## FIXME Add DESeqAnalysisList method support?
## FIXME Add DESeqResultsList method support.



#' @name resultsNames
#' @inherit AcidGenerics::resultsNames
#' @note Updated 2019-09-10.
#'
#' @inheritParams AcidRoxygen::params
#' @inheritParams params
#' @param ... Additional arguments.
#'
#' @examples
#' data(deseq)
#'
#' ## DESeqAnalysis ====
#' resultsNames(deseq)
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



## Updated 2019-09-10.
`resultsNames<-,DESeqAnalysis,character` <-  # nolint
    function(object, value) {
        names(object@results) <- value
        if (!is.null(object@lfcShrink)) {
            names(object@lfcShrink) <- value
        }
        validObject(object)
        object
    }



#' @rdname resultsNames
#' @export
setReplaceMethod(
    f = "resultsNames",
    signature = signature(
        object = "DESeqAnalysis",
        value = "character"
    ),
    definition = `resultsNames<-,DESeqAnalysis,character`
)
