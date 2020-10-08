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



#' @rdname resultsNames
#' @name resultsNames
#' @importFrom AcidGenerics resultsNames
#' @usage resultsNames(object, ...)
#' @export
NULL

#' @rdname resultsNames
#' @name resultsNames<-
#' @importFrom AcidGenerics resultsNames<-
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




## contrastNames aliases =======================================================
#' @rdname resultsNames
#' @name contrastNames
#' @importFrom AcidGenerics contrastNames
#' @usage contrastNames(object, ...)
#' @export
NULL

#' @rdname resultsNames
#' @name contrastNames<-
#' @importFrom AcidGenerics contrastNames<-
#' @usage contrastNames(object, ...) <- value
#' @export
NULL



`contrastNames,DESeqDataSet` <-  # nolint
    `resultsNames,DESeqDataSet`



#' @rdname resultsNames
#' @export
setMethod(
    f = "contrastNames",
    signature = signature("DESeqDataSet"),
    definition = `contrastNames,DESeqDataSet`
)



## Updated 2019-07-23.
`contrastNames,DESeqAnalysis` <-  # nolint
    `resultsNames,DESeqAnalysis`



#' @rdname resultsNames
#' @export
setMethod(
    f = "contrastNames",
    signature = signature("DESeqAnalysis"),
    definition = `contrastNames,DESeqAnalysis`
)



## Updated 2019-09-10.
`contrastNames<-,DESeqAnalysis,character` <-  # nolint
    `resultsNames<-,DESeqAnalysis,character`



#' @rdname resultsNames
#' @export
setReplaceMethod(
    f = "contrastNames",
    signature = signature(
        object = "DESeqAnalysis",
        value = "character"
    ),
    definition = `contrastNames<-,DESeqAnalysis,character`
)
