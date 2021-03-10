#' @name alphaThreshold
#' @inherit AcidGenerics::alphaThreshold
#' @note Updated 2021-03-10.
#'
#' @details
#' Assumes `0.01` by default if unset.
#'
#' @param ... Additional arguments.
#'
#' @examples
#' data(deseq)
#'
#' ## DESeqAnalysis ====
#' object <- deseq
#' alphaThreshold(object) <- 0.05
#' alphaThreshold(object)
NULL



## Updated 2021-03-10.
`alphaThreshold,DESeqAnalysis` <-  # nolint
    function(object) {
        x <- metadata(object)[["alphaThreshold"]]
        if (is.null(x)) {
            resList <- DESeqResultsList(object, quiet = TRUE)
            x <- alphaThreshold(resList)
        }
        assert(isAlpha(x))
        x
    }



## Updated 2021-03-10.
`alphaThreshold,DESeqAnalysisList` <-  # nolint
    function(object) {
        assert(hasLength(object))
        x <- metadata(object)[["alphaThreshold"]]
        if (is.null(x)) {
            x <- alphaThreshold(object[[1L]])
        }
        assert(isAlpha(x))
        x
    }



## Other methods slot "alphaThreshold" instead of "alpha".
## Updated 2021-03-03.
`alphaThreshold,DESeqResults` <-  # nolint
    function(object) {
        x <- metadata(object)[["alpha"]]
        if (is.null(x)) x <- 0.01
        assert(isAlpha(x))
        x
    }



## Updated 2021-03-10.
`alphaThreshold,DESeqResultsList` <-  # nolint
    function(object) {
        assert(hasLength(object))
        x <- metadata(object)[["alphaThreshold"]]
        if (is.null(x)) {
            x <- alphaThreshold(object[[1L]])
        }
        x
    }



## Updated 2020-08-04.
`alphaThreshold<-,DESeqAnalysis,numeric` <-  # nolint
    function(object, value) {
        assert(isAlpha(value))
        metadata(object)[["alphaThreshold"]] <- value
        object
    }



## Updated 2020-08-04.
`alphaThreshold<-,DESeqAnalysis,NULL` <-  # nolint
    function(object, value) {
        metadata(object)[["alphaThreshold"]] <- value
        object
    }



## Updated 2021-03-10.
`alphaThreshold<-,DESeqAnalysisList,numeric` <-  # nolint
    `alphaThreshold<-,DESeqAnalysis,numeric`



## Updated 2021-03-10.
`alphaThreshold<-,DESeqAnalysisList,NULL` <-  # nolint
    `alphaThreshold<-,DESeqAnalysis,NULL`



## Updated 2021-03-03.
`alphaThreshold<-,DESeqResults,numeric` <-  # nolint
    function(object, value) {
        assert(isAlpha(value))
        metadata(object)[["alpha"]] <- value
        object
    }



## Updated 2021-03-10.
`alphaThreshold<-,DESeqResultsList,numeric` <-  # nolint
    `alphaThreshold<-,DESeqAnalysisList,numeric`



## Updated 2021-03-10.
`alphaThreshold<-,DESeqResultsList,NULL` <-  # nolint
    `alphaThreshold<-,DESeqAnalysisList,NULL`



#' @rdname alphaThreshold
#' @export
setMethod(
    f = "alphaThreshold",
    signature = signature("DESeqAnalysis"),
    definition = `alphaThreshold,DESeqAnalysis`
)



#' @rdname alphaThreshold
#' @export
setMethod(
    f = "alphaThreshold",
    signature = signature("DESeqAnalysisList"),
    definition = `alphaThreshold,DESeqAnalysisList`
)



#' @rdname alphaThreshold
#' @export
setMethod(
    f = "alphaThreshold",
    signature = signature("DESeqResults"),
    definition = `alphaThreshold,DESeqResults`
)



#' @rdname alphaThreshold
#' @export
setMethod(
    f = "alphaThreshold",
    signature = signature("DESeqResultsList"),
    definition = `alphaThreshold,DESeqResultsList`
)



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



#' @rdname alphaThreshold
#' @export
setReplaceMethod(
    f = "alphaThreshold",
    signature = signature(
        object = "DESeqAnalysisList",
        value = "numeric"
    ),
    definition = `alphaThreshold<-,DESeqAnalysisList,numeric`
)



#' @rdname alphaThreshold
#' @export
setReplaceMethod(
    f = "alphaThreshold",
    signature = signature(
        object = "DESeqAnalysisList",
        value = "NULL"
    ),
    definition = `alphaThreshold<-,DESeqAnalysisList,NULL`
)



#' @rdname alphaThreshold
#' @export
setReplaceMethod(
    f = "alphaThreshold",
    signature = signature(
        object = "DESeqResults",
        value = "numeric"
    ),
    definition = `alphaThreshold<-,DESeqResults,numeric`
)



## Intentionally not allowing NULL method for DESeqResults here.



#' @rdname alphaThreshold
#' @export
setReplaceMethod(
    f = "alphaThreshold",
    signature = signature(
        object = "DESeqResultsList",
        value = "numeric"
    ),
    definition = `alphaThreshold<-,DESeqResultsList,numeric`
)



#' @rdname alphaThreshold
#' @export
setReplaceMethod(
    f = "alphaThreshold",
    signature = signature(
        object = "DESeqResultsList",
        value = "NULL"
    ),
    definition = `alphaThreshold<-,DESeqResultsList,NULL`
)
