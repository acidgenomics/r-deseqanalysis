#' @name plotHeatmap
#' @inherit acidplots::plotHeatmap
#' @note Updated 2019-11-07.
#'
#' @inheritParams acidroxygen::params
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



#' @rdname plotHeatmap
#' @name plotHeatmap
#' @importFrom bioverbs plotHeatmap
#' @usage plotHeatmap(object, ...)
#' @export
NULL

#' @rdname plotHeatmap
#' @name plotCorrelationHeatmap
#' @importFrom bioverbs plotCorrelationHeatmap
#' @usage plotCorrelationHeatmap(object, ...)
#' @export
NULL

#' @rdname plotHeatmap
#' @name plotQuantileHeatmap
#' @importFrom bioverbs plotQuantileHeatmap
#' @usage plotQuantileHeatmap(object, ...)
#' @export
NULL



`plotHeatmap,DESeqAnalysis` <-  # nolint
    function(object, ...) {
        validObject(object)
        message("Using DESeqTransform ", transformType(object), " counts.")
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
        message("Using DESeqTransform ", transformType(object), " counts.")
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
        message("Using DESeqTransform ", transformType(object), " counts.")
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
