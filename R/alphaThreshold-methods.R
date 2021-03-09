## FIXME NEED TO RETHINK THIS WITH DESEQRESULTSLIST.
## FIXME NEED TO RETHINK THIS WITH DESEQANALYSISLIST SUPPORT.



#' @name alphaThreshold
#' @inherit AcidGenerics::alphaThreshold
#' @note Updated 2021-03-03.
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



## Updated 2021-03-03.
`alphaThreshold,DESeqResults` <-  # nolint
    function(object) {
        x <- metadata(object)[["alpha"]]
        if (is.null(x)) x <- 0.01
        assert(isAlpha(x))
        x
    }



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



## Updated 2021-03-03.
`alphaThreshold<-,DESeqResults,numeric` <-  # nolint
    function(object, value) {
        assert(isAlpha(value))
        metadata(object)[["alpha"]] <- value
        object
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
    signature = signature("DESeqAnalysis"),
    definition = `alphaThreshold,DESeqAnalysis`
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
