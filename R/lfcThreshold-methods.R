#' @name lfcThreshold
#' @inherit AcidGenerics::lfcThreshold
#' @note Updated 2019-08-06.
#' @param ... Additional arguments.
#' @examples
#' data(deseq)
#'
#' ## DESeqAnalysis ====
#' lfcThreshold(deseq) <- 0.2
#' lfcThreshold(deseq)
NULL



## Updated 2020-08-04.
`lfcThreshold,DESeqResults` <-  # nolint
    function(object) {
        x <- metadata(object)[["lfcThreshold"]]
        assert(isNumber(x), isNonNegative(x))
        x
    }



#' @rdname lfcThreshold
#' @export
setMethod(
    f = "lfcThreshold",
    signature = signature("DESeqResults"),
    definition = `lfcThreshold,DESeqResults`
)



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



#' @rdname lfcThreshold
#' @export
setMethod(
    f = "lfcThreshold",
    signature = signature("DESeqAnalysis"),
    definition = `lfcThreshold,DESeqAnalysis`
)



## Updated 2020-08-04.
`lfcThreshold<-,DESeqAnalysis,numeric` <-  # nolint
    function(object, value) {
        assert(isScalar(value), isNonNegative(value))
        metadata(object)[["lfcThreshold"]] <- value
        object
    }



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



## Updated 2020-08-04.
`lfcThreshold<-,DESeqAnalysis,NULL` <-  # nolint
    function(object, value) {
        metadata(object)[["lfcThreshold"]] <- value
        object
    }



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
