#' @name plotHeatmap
#' @inherit AcidPlots::plotHeatmap
#' @note Updated 2020-08-04.
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



`plotHeatmap,DESeqAnalysis` <-  # nolint
    function(object, ...) {
        validObject(object)
        alertInfo(paste0(
            "Using {.var DESeqTransform} ",
            "{.var ", transformType(object), "} counts."
        ))
        dt <- as(object, "DESeqTransform")
        plotHeatmap(dt, ...)
    }



#' @rdname plotHeatmap
#' @export
setMethod(
    f = "plotHeatmap",
    signature = signature("DESeqAnalysis"),
    definition = `plotHeatmap,DESeqAnalysis`
)



`plotCorrelationHeatmap,DESeqAnalysis` <-  # nolint
    function(object, ...) {
        validObject(object)
        alertInfo(paste0(
            "Using {.var DESeqTransform} ",
            "{.var ", transformType(object), "} counts."
        ))
        dt <- as(object, "DESeqTransform")
        plotCorrelationHeatmap(dt, ...)
    }



#' @rdname plotHeatmap
#' @export
setMethod(
    f = "plotCorrelationHeatmap",
    signature = signature("DESeqAnalysis"),
    definition = `plotCorrelationHeatmap,DESeqAnalysis`
)



`plotQuantileHeatmap,DESeqAnalysis` <-  # nolint
    function(object, ...) {
        validObject(object)
        alertInfo(paste0(
            "Using {.var DESeqTransform} ",
            "{.var ", transformType(object), "} counts."
        ))
        dt <- as(object, "DESeqTransform")
        plotQuantileHeatmap(dt, ...)
    }



#' @rdname plotHeatmap
#' @export
setMethod(
    f = "plotQuantileHeatmap",
    signature = signature("DESeqAnalysis"),
    definition = `plotQuantileHeatmap,DESeqAnalysis`
)
