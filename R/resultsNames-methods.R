#' @name resultsNames
#' @inherit bioverbs::resultsNames
#' @note Updated 2019-09-10.
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

#' @rdname resultsNames
#' @name resultsNames<-
#' @importFrom bioverbs resultsNames<-
#' @usage resultsNames(object, ...) <- value
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



#' @rdname sampleData
#' @export
setReplaceMethod(
    f = "resultsNames",
    signature = signature(
        object = "DESeqAnalysis",
        value = "character"
    ),
    definition = `resultsNames<-,DESeqAnalysis,character`
)
