#' @name resultsNames
#' @inherit AcidGenerics::resultsNames
#' @note Updated 2021-03-15.
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



## Updated 2021-03-15.
`resultsNames,DESeqAnalysis` <-  # nolint
    function(object) {
        resList <- DESeqResultsList(object, quiet = TRUE)
        names(resList)
    }



## Updated 2021-03-15.
`resultsNames,DESeqAnalysisList` <-  # nolint
    `resultsNames,DESeqAnalysis`



## Updated 2019-07-23.
`resultsNames,DESeqDataSet` <-  # nolint
    function(object) {
        DESeq2::resultsNames(object)
    }



## Updated 2021-03-15.
`resultsNames,DESeqResultsList` <-  # nolint
    function(object) {
        names(object)
    }



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



## Updated 2021-03-15.
`resultsNames<-,DESeqResultsList,character` <-  # nolint
    function(object, value) {
        names(object) <- value
        validObject(object)
        object
    }



#' @rdname resultsNames
#' @export
setMethod(
    f = "resultsNames",
    signature = signature(object = "DESeqAnalysis"),
    definition = `resultsNames,DESeqAnalysis`
)

#' @rdname resultsNames
#' @export
setMethod(
    f = "resultsNames",
    signature = signature(object = "DESeqAnalysisList"),
    definition = `resultsNames,DESeqAnalysisList`
)

#' @rdname resultsNames
#' @export
setMethod(
    f = "resultsNames",
    signature = signature(object = "DESeqDataSet"),
    definition = `resultsNames,DESeqDataSet`
)

#' @rdname resultsNames
#' @export
setMethod(
    f = "resultsNames",
    signature = signature(object = "DESeqResultsList"),
    definition = `resultsNames,DESeqResultsList`
)



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

#' @rdname resultsNames
#' @export
setReplaceMethod(
    f = "resultsNames",
    signature = signature(
        object = "DESeqResultsList",
        value = "character"
    ),
    definition = `resultsNames<-,DESeqResultsList,character`
)
