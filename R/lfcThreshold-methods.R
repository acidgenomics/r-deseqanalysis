## FIXME Match the method support to alphaThreshold.



#' @name lfcThreshold
#' @inherit AcidGenerics::lfcThreshold
#' @note Updated 2021-03-12.
#'
#' @details
#' Assumes `0` by default if unset.
#'
#' @param ... Additional arguments.
#'
#' @examples
#' data(deseq)
#'
#' ## DESeqAnalysis ====
#' lfcThreshold(deseq) <- 0.2
#' lfcThreshold(deseq)
NULL



## Updated 2020-08-04.
`lfcThreshold,DESeqAnalysis` <-  # nolint
    function(object) {
        x <- metadata(object)[["lfcThreshold"]]
        if (is.null(x)) {
            x <- lfcThreshold(slot(object, "results")[[1L]])
        }
        assert(isNumber(x), isNonNegative(x))
        x
    }



## Updated 2021-03-12.
`lfcThreshold,DESeqAnalysisList` <-  # nolint
    function(object) {
        assert(hasLength(object))
        lfcThreshold(object[[1L]])
    }



## Updated 2021-03-03.
`lfcThreshold,DESeqResults` <-  # nolint
    function(object) {
        x <- metadata(object)[["lfcThreshold"]]
        if (is.null(x)) {
            x <- 0L
        }
        assert(isNumber(x), isNonNegative(x))
        x
    }



## Updated 2021-03-12.
`lfcThreshold,DESeqResultsList` <-  # nolint
    function(object) {
        assert(hasLength(object))
        lfcThreshold(object[[1L]])
    }



## Updated 2021-03-03.
`lfcThreshold<-,DESeqAnalysis,numeric` <-  # nolint
    function(object, value) {
        assert(isScalar(value), isNonNegative(value))
        metadata(object)[["lfcThreshold"]] <- value
        object
    }



## Updated 2020-08-04.
`lfcThreshold<-,DESeqAnalysis,NULL` <-  # nolint
    function(object, value) {
        metadata(object)[["lfcThreshold"]] <- value
        object
    }



## Updated 2021-03-12.
`lfcThreshold<-,DESeqResults,numeric` <-  # nolint
    `lfcThreshold<-,DESeqAnalysis,numeric`



#' @rdname lfcThreshold
#' @export
setMethod(
    f = "lfcThreshold",
    signature = signature("DESeqAnalysis"),
    definition = `lfcThreshold,DESeqAnalysis`
)



#' @rdname lfcThreshold
#' @export
setMethod(
    f = "lfcThreshold",
    signature = signature("DESeqAnalysisList"),
    definition = `lfcThreshold,DESeqAnalysisList`
)



#' @rdname lfcThreshold
#' @export
setMethod(
    f = "lfcThreshold",
    signature = signature("DESeqResults"),
    definition = `lfcThreshold,DESeqResults`
)



#' @rdname lfcThreshold
#' @export
setMethod(
    f = "lfcThreshold",
    signature = signature("DESeqResultsList"),
    definition = `lfcThreshold,DESeqResultsList`
)



#' @rdname lfcThreshold
#' @export
setReplaceMethod(
    f = "lfcThreshold",
    signature = signature(
        object = "DESeqAnalysis",
        value = "numeric"
    ),
    definition = `lfcThreshold<-,DESeqAnalysis,numeric`
)



#' @rdname lfcThreshold
#' @export
setReplaceMethod(
    f = "lfcThreshold",
    signature = signature(
        object = "DESeqAnalysis",
        value = "NULL"
    ),
    definition = `lfcThreshold<-,DESeqAnalysis,NULL`
)



#' @rdname lfcThreshold
#' @export
setReplaceMethod(
    f = "lfcThreshold",
    signature = signature(
        object = "DESeqResults",
        value = "numeric"
    ),
    definition = `lfcThreshold<-,DESeqResults,numeric`
)
