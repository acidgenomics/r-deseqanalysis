#' @name plotHeatmap
#' @inherit AcidPlots::plotHeatmap
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
#' plotHeatmap(deseq)
#' plotCorrelationHeatmap(deseq)
#' plotQuantileHeatmap(deseq)
NULL



## Updated 2021-03-15.
`plotHeatmap,DESeqAnalysis` <- # nolint
    function(object, ...) {
        assert(validObject(object))
        alertInfo(sprintf(
            "Using {.var %s} {.var %s} counts.",
            "DESeqTransform", transformType(object)
        ))
        dt <- as(object, "DESeqTransform")
        plotHeatmap(dt, ...)
    }



## Updated 2021-03-15.
`plotCorrelationHeatmap,DESeqAnalysis` <- # nolint
    function(object, ...) {
        assert(validObject(object))
        alertInfo(sprintf(
            "Using {.var %s} {.var %s} counts.",
            "DESeqTransform", transformType(object)
        ))
        dt <- as(object, "DESeqTransform")
        plotCorrelationHeatmap(dt, ...)
    }



## Updated 2021-03-15.
`plotQuantileHeatmap,DESeqAnalysis` <- # nolint
    function(object, ...) {
        assert(validObject(object))
        alertInfo(sprintf(
            "Using {.var %s} {.var %s} counts.",
            "DESeqTransform", transformType(object)
        ))
        dt <- as(object, "DESeqTransform")
        plotQuantileHeatmap(dt, ...)
    }



#' @rdname plotHeatmap
#' @export
setMethod(
    f = "plotHeatmap",
    signature = signature(object = "DESeqAnalysis"),
    definition = `plotHeatmap,DESeqAnalysis`
)



#' @rdname plotHeatmap
#' @export
setMethod(
    f = "plotCorrelationHeatmap",
    signature = signature(object = "DESeqAnalysis"),
    definition = `plotCorrelationHeatmap,DESeqAnalysis`
)



#' @rdname plotHeatmap
#' @export
setMethod(
    f = "plotQuantileHeatmap",
    signature = signature(object = "DESeqAnalysis"),
    definition = `plotQuantileHeatmap,DESeqAnalysis`
)
