#' @name alphaThreshold
#' @inherit acidgenerics::alphaThreshold
#' @note Updated 2019-08-06.
#' @param ... Additional arguments.
#' @examples
#' data(RangedSummarizedExperiment, package = "acidtest")
#' rse <- RangedSummarizedExperiment
#'
#' ## Annotated ====
#' alphaThreshold(rse) <- 0.05
#' alphaThreshold(rse)
NULL



#' @rdname alphaThreshold
#' @name alphaThreshold
#' @importFrom acidgenerics alphaThreshold
#' @usage alphaThreshold(object, ...)
#' @export
NULL

#' @rdname alphaThreshold
#' @name alphaThreshold<-
#' @importFrom acidgenerics alphaThreshold<-
#' @usage alphaThreshold(object, ...) <- value
#' @export
NULL



## Updated 2020-08-04.
`alphaThreshold,DESeqResults` <-  # nolint
    function(object) {
        validObject(object)
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



`alphaThreshold,DESeqAnalysis` <-  # nolint
    function(object) {
        validObject(object)
        x <- metadata(object)[["alpha"]]
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
        metadata(object)[["alpha"]] <- value
        validObject(object)
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
