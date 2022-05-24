#' @export
#' @rdname DESeqAnalysisList
setGeneric(
    name = "DESeqAnalysisList",
    def = function(object, ...) {
        standardGeneric("DESeqAnalysisList")
    }
)

#' @export
#' @rdname DESeqResultsList
setGeneric(
    name = "DESeqResultsList",
    def = function(object, ...) {
        standardGeneric("DESeqResultsList")
    }
)

#' @export
#' @name alphaSummary
#' @rdname alphaSummary
#' @usage alphaSummary(object, ...)
NULL

#' @aliases alphaThreshold<-
#' @export alphaThreshold alphaThreshold<-
#' @name alphaThreshold
#' @rdname alphaThreshold
#' @usage
#' alphaThreshold(object, ...)
#' alphaThreshold(object, ...) <- value
NULL

#' @export
#' @rdname apeglmResults
setGeneric(
    name = "apeglmResults",
    def = function(object, ...) {
        standardGeneric("apeglmResults")
    }
)

#' @export
#' @name as.DESeqDataSet
#' @rdname coerce
#' @usage as.DESeqDataSet(x, ...)
NULL

#' @export
#' @name as.DESeqTransform
#' @rdname coerce
#' @usage as.DESeqTransform(x, ...)
NULL

#' @aliases baseMeanThreshold<-
#' @export baseMeanThreshold baseMeanThreshold<-
#' @name baseMeanThreshold
#' @rdname baseMeanThreshold
#' @usage
#' baseMeanThreshold(object, ...)
#' baseMeanThreshold(object, ...) <- value
NULL

#' @export
#' @name combine
#' @rdname combine
#' @usage combine(x, y, ...)
NULL

#' @aliases contrastName<-
#' @export contrastName contrastName<-
#' @name contrastName
#' @rdname contrastName
#' @usage
#' contrastName(object, ...)
#' contrastName(object, ...) <- value
NULL

#' @export
#' @name contrastSamples
#' @rdname contrastSamples
#' @usage contrastSamples(object, ...)
NULL

#' @export
#' @name correlation
#' @rdname correlation
#' @usage correlation(x, y, ...)
NULL

#' @export
#' @name deg
#' @rdname deg
#' @usage deg(object, ...)
NULL

#' @export
#' @name degIntersection
#' @rdname degIntersection
#' @usage degIntersection(object, ...)
NULL

#' @export
#' @name degPerContrast
#' @rdname degPerContrast
#' @usage degPerContrast(object, ...)
NULL

#' @export
#' @name export
#' @rdname export
#' @usage export(object, con, format, ...)
NULL

#' @aliases interestingGroups<-
#' @export interestingGroups interestingGroups<-
#' @name interestingGroups
#' @rdname interestingGroups
#' @usage
#' interestingGroups(object, ...)
#' interestingGroups(object, ...)  <- value
NULL

#' @aliases lfcShrink<-
#' @export lfcShrink lfcShrink<-
#' @name lfcShrink
#' @rdname lfcShrink
#' @usage
#' lfcShrink(object, ...)
#' lfcShrink(object, ...) <- value
NULL

#' @export
#' @name lfcShrinkType
#' @rdname lfcShrinkType
#' @usage lfcShrinkType(object, ...)
NULL

#' @aliases lfcThreshold<-
#' @export lfcThreshold lfcThreshold<-
#' @name lfcThreshold
#' @rdname lfcThreshold
#' @usage
#' lfcThreshold(object, ...)
#' lfcThreshold(object, ...) <- value
NULL

#' @export
#' @name markdownTables
#' @rdname markdownTables
#' @usage markdownTables(object, ...)
NULL

#' @export
#' @name plotBaseMean
#' @rdname plotBaseMean
#' @usage plotBaseMean(object, ...)
NULL

#' @export
#' @name plotContrastScatter
#' @rdname plotContrastScatter
#' @usage plotContrastScatter(object, ...)
NULL

#' @export
#' @name plotCorrelationHeatmap
#' @rdname plotHeatmap
#' @usage plotCorrelationHeatmap(object, ...)
NULL

#' @export
#' @name plotCounts
#' @rdname plotCounts
#' @usage plotCounts(object, ...)
NULL

#' @export
#' @name plotDEGHeatmap
#' @rdname plotDEGHeatmap
#' @usage plotDEGHeatmap(object, ...)
NULL

#' @export
#' @name plotDEGPCA
#' @rdname plotDEGPCA
#' @usage plotDEGPCA(object, ...)
NULL

#' @export
#' @name plotDEGStackedBar
#' @rdname plotDEGStackedBar
#' @usage plotDEGStackedBar(object, ...)
NULL

#' @export
#' @name plotDEGUpset
#' @rdname plotDEGUpset
#' @usage plotDEGUpset(object, ...)
NULL

#' @export
#' @name plotHeatmap
#' @rdname plotHeatmap
#' @usage plotHeatmap(object, ...)
NULL

#' @export
#' @name plotLFC
#' @rdname plotLFC
#' @usage plotLFC(object, ...)
NULL

#' @export
#' @name plotMA
#' @rdname plotMA
#' @usage plotMA(object, ...)
NULL

#' @export
#' @name plotPCA
#' @rdname plotPCA
#' @usage plotPCA(object, ...)
NULL

#' @export
#' @name plotQuantileHeatmap
#' @rdname plotHeatmap
#' @usage plotQuantileHeatmap(object, ...)
NULL

#' @export
#' @name plotVolcano
#' @rdname plotVolcano
#' @usage plotVolcano(object, ...)
NULL

#' @export
#' @name results
#' @rdname results
#' @usage results(object, ...)
NULL

#' @export
#' @name resultsDiff
#' @rdname resultsDiff
#' @usage resultsDiff(x, y, ...)
NULL

#' @export
#' @name resultsMatrix
#' @rdname resultsMatrix
#' @usage resultsMatrix(object, ...)
NULL

#' @aliases resultsNames<-
#' @export resultsNames resultsNames<-
#' @name resultsNames
#' @rdname resultsNames
#' @usage
#' resultsNames(object, ...)
#' resultsNames(object, ...) <- value
NULL

#' @export
#' @name resultsTables
#' @rdname resultsTables
#' @usage resultsTables(object, ...)
NULL

#' @export
#' @name sampleData
#' @rdname sampleData
#' @usage sampleData(object, ...)
NULL

#' @export
#' @name show
#' @rdname show
#' @usage show(object)
NULL

#' @export
#' @name transformType
#' @rdname transformType
#' @usage transformType(object, ...)
NULL

#' @export
#' @name updateObject
#' @rdname updateObject
#' @usage updateObject(object, ..., verbose = FALSE)
NULL
