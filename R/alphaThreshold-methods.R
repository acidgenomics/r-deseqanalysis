#' @name alphaThreshold
#' @inherit AcidGenerics::alphaThreshold
#' @note Updated 2020-08-04.
#' @param ... Additional arguments.
#' @examples
#' data(deseq)
#'
#' ## DESeqAnalysis ====
#' object <- deseq
#' alphaThreshold(object) <- 0.05
#' alphaThreshold(object)
NULL



#' @rdname alphaThreshold
#' @name alphaThreshold
#' @importFrom AcidGenerics alphaThreshold
#' @usage alphaThreshold(object, ...)
#' @export
NULL

#' @rdname alphaThreshold
#' @name alphaThreshold<-
#' @importFrom AcidGenerics alphaThreshold<-
#' @usage alphaThreshold(object, ...) <- value
#' @export
NULL



## Updated 2020-08-04.
`alphaThreshold,DESeqResults` <-  # nolint
    function(object) {
        x <- metadata(object)[["alpha"]]
        assert(isAlpha(x))
        x
    }



#' @rdname alphaThreshold
#' @export
setMethod(
    f = "alphaThreshold",
    signature = signature("DESeqResults"),
    definition = `alphaThreshold,DESeqResults`
)



## Updated 2020-08-04.
`alphaThreshold,DESeqAnalysis` <-  # nolint
    function(object) {
        x <- metadata(object)[["alphaThreshold"]]
        if (is.null(x)) {
            x <- alphaThreshold(slot(object, "results")[[1L]])
        }
        assert(isAlpha(x))
        x
    }



#' @rdname alphaThreshold
#' @export
setMethod(
    f = "alphaThreshold",
    signature = signature("DESeqAnalysis"),
    definition = `alphaThreshold,DESeqAnalysis`
)



## Updated 2020-08-04.
`alphaThreshold<-,DESeqAnalysis,numeric` <-  # nolint
    function(object, value) {
        assert(isAlpha(value))
        metadata(object)[["alphaThreshold"]] <- value
        object
    }



#' @rdname alphaThreshold
#' @export
setReplaceMethod(
    f = "alphaThreshold",
    signature = signature(
        object = "DESeqAnalysis",
        value = "numeric"
    ),
    definition = `alphaThreshold<-,DESeqAnalysis,numeric`
)



## Updated 2020-08-04.
`alphaThreshold<-,DESeqAnalysis,NULL` <-  # nolint
    function(object, value) {
        metadata(object)[["alphaThreshold"]] <- value
        object
    }



#' @rdname alphaThreshold
#' @export
setReplaceMethod(
    f = "alphaThreshold",
    signature = signature(
        object = "DESeqAnalysis",
        value = "NULL"
    ),
    definition = `alphaThreshold<-,DESeqAnalysis,NULL`
)
