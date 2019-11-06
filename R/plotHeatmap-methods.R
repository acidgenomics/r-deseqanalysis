# plotHeatmap
# plotCorrelationHeatmap
# plotQuantileHeatmap



`plotHeatmap,DESeqAnalysis` <-  # nolint
    function(object, ...) {
        validObject(object)
        dt <- as(object, "DESeqTransform")
        plotHeatmap(dt, ...)
    }



`plotCorrelationHeatmap,DESeqAnalysis` <-  # nolint
    function(object, ...) {
        validObject(object)
        dt <- as(object, "DESeqTransform")
        plotCorrelationHeatmap(dt, ...)
    }



`plotQuantileHeatmap,DESeqAnalysis` <-  # nolint
    function(object, ...) {
        validObject(object)
        dt <- as(object, "DESeqTransform")
        plotQuantileHeatmap(dt, ...)
    }
