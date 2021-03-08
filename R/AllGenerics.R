#' @rdname DESeqAnalysisList
#' @export
setGeneric(
    name = "DESeqAnalysisList",
    def = function(object, ...) {
        standardGeneric("DESeqAnalysisList")
    }
)



#' @rdname DESeqResultsList
#' @export
setGeneric(
    name = "DESeqResultsList",
    def = function(object, ...) {
        standardGeneric("DESeqResultsList")
    }
)



#' @rdname apeglmResults
#' @export
setGeneric(
    name = "apeglmResults",
    def = function(object, ...) {
        standardGeneric("apeglmResults")
    }
)



#' @rdname alphaSummary
#' @name alphaSummary
#' @importFrom AcidGenerics alphaSummary
#' @usage alphaSummary(object, ...)
#' @export
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



#' @rdname baseMeanThreshold
#' @name baseMeanThreshold
#' @importFrom AcidGenerics baseMeanThreshold
#' @usage baseMeanThreshold(object, ...)
#' @export
NULL

#' @rdname baseMeanThreshold
#' @name baseMeanThreshold<-
#' @importFrom AcidGenerics baseMeanThreshold<-
#' @usage baseMeanThreshold(object, ...) <- value
#' @export
NULL



#' @rdname combine
#' @name combine
#' @importFrom AcidGenerics combine
#' @usage combine(x, y, ...)
#' @export
NULL



#' @rdname contrastName
#' @name contrastName
#' @importFrom AcidGenerics contrastName
#' @usage contrastName(object, ...)
#' @export
NULL

#' @rdname contrastName
#' @name contrastName<-
#' @importFrom AcidGenerics contrastName<-
#' @usage contrastName(object, ...) <- value
#' @export
NULL



#' @rdname contrastSamples
#' @name contrastSamples
#' @importFrom AcidGenerics contrastSamples
#' @usage contrastSamples(object, ...)
#' @export
NULL



#' @rdname correlation
#' @name correlation
#' @importFrom basejump correlation
#' @usage correlation(x, y, ...)
#' @export
NULL



#' @rdname deg
#' @name deg
#' @importFrom AcidGenerics deg
#' @usage deg(object, ...)
#' @export
NULL



#' @rdname degIntersection
#' @name degIntersection
#' @importFrom AcidGenerics degIntersection
#' @usage degIntersection(object, ...)
#' @export
NULL



#' @rdname degPerContrast
#' @name degPerContrast
#' @importFrom AcidGenerics degPerContrast
#' @usage degPerContrast(object, ...)
#' @export
NULL



#' @rdname export
#' @name export
#' @importFrom AcidGenerics export
#' @usage export(object, ...)
#' @export
NULL



#' @rdname interestingGroups
#' @name interestingGroups
#' @importFrom basejump interestingGroups
#' @usage interestingGroups(object, ...)
#' @export
NULL

#' @rdname interestingGroups
#' @name interestingGroups<-
#' @importFrom basejump interestingGroups<-
#' @usage interestingGroups(object, ...)  <- value
#' @export
NULL



#' @rdname lfcShrink
#' @name lfcShrink
#' @importFrom AcidGenerics lfcShrink
#' @usage lfcShrink(object, ...)
#' @export
NULL

#' @rdname lfcShrink
#' @name lfcShrink<-
#' @importFrom AcidGenerics lfcShrink<-
#' @usage lfcShrink(object, ...) <- value
#' @export
NULL



#' @rdname lfcShrinkType
#' @name lfcShrinkType
#' @importFrom AcidGenerics lfcShrinkType
#' @usage lfcShrinkType(object, ...)
#' @export
NULL



#' @rdname lfcThreshold
#' @name lfcThreshold
#' @importFrom AcidGenerics lfcThreshold
#' @usage lfcThreshold(object, ...)
#' @export
NULL

#' @rdname lfcThreshold
#' @name lfcThreshold<-
#' @importFrom AcidGenerics lfcThreshold<-
#' @usage lfcThreshold(object, ...) <- value
#' @export
NULL



#' @rdname plotBaseMean
#' @name plotBaseMean
#' @importFrom AcidGenerics plotBaseMean
#' @usage plotBaseMean(object, ...)
#' @export
NULL



#' @rdname plotCounts
#' @name plotCounts
#' @importFrom AcidGenerics plotCounts
#' @usage plotCounts(object, ...)
#' @export
NULL



#' @rdname plotDEGHeatmap
#' @name plotDEGHeatmap
#' @importFrom AcidGenerics plotDEGHeatmap
#' @usage plotDEGHeatmap(object, ...)
#' @export
NULL



#' @rdname plotDEGPCA
#' @name plotDEGPCA
#' @importFrom AcidGenerics plotDEGPCA
#' @usage plotDEGPCA(object, ...)
#' @export
NULL



#' @rdname plotDEGStackedBar
#' @name plotDEGStackedBar
#' @importFrom AcidGenerics plotDEGStackedBar
#' @usage plotDEGStackedBar(object, ...)
#' @export
NULL



#' @rdname plotDEGUpset
#' @name plotDEGUpset
#' @importFrom AcidGenerics plotDEGUpset
#' @usage plotDEGUpset(object, ...)
#' @export
NULL



#' @rdname plotHeatmap
#' @name plotHeatmap
#' @importFrom AcidGenerics plotHeatmap
#' @usage plotHeatmap(object, ...)
#' @export
NULL

#' @rdname plotHeatmap
#' @name plotCorrelationHeatmap
#' @importFrom AcidGenerics plotCorrelationHeatmap
#' @usage plotCorrelationHeatmap(object, ...)
#' @export
NULL

#' @rdname plotHeatmap
#' @name plotQuantileHeatmap
#' @importFrom AcidGenerics plotQuantileHeatmap
#' @usage plotQuantileHeatmap(object, ...)
#' @export
NULL



#' @rdname plotMA
#' @name plotMA
#' @importFrom AcidGenerics plotMA
#' @usage plotMA(object, ...)
#' @export
NULL



#' @rdname plotPCA
#' @name plotPCA
#' @importFrom AcidGenerics plotPCA
#' @usage plotPCA(object, ...)
#' @export
NULL



#' @rdname plotVolcano
#' @name plotVolcano
#' @importFrom AcidGenerics plotVolcano
#' @usage plotVolcano(object, ...)
#' @export
NULL



#' @rdname results
#' @name results
#' @importFrom AcidGenerics results
#' @usage results(object, ...)
#' @export
NULL



#' @rdname resultsDiff
#' @name resultsDiff
#' @importFrom AcidGenerics resultsDiff
#' @usage resultsDiff(x, y, ...)
#' @export
NULL



#' @rdname resultsMatrix
#' @name resultsMatrix
#' @importFrom AcidGenerics resultsMatrix
#' @usage resultsMatrix(object, ...)
#' @export
NULL



#' @rdname resultsNames
#' @name resultsNames
#' @importFrom AcidGenerics resultsNames
#' @usage resultsNames(object, ...)
#' @export
NULL

#' @rdname resultsNames
#' @name resultsNames<-
#' @importFrom AcidGenerics resultsNames<-
#' @usage resultsNames(object, ...) <- value
#' @export
NULL



#' @rdname resultsTables
#' @name resultsTables
#' @importFrom AcidGenerics resultsTables
#' @usage resultsTables(object, ...)
#' @export
NULL



#' @rdname sampleData
#' @name sampleData
#' @importFrom AcidGenerics sampleData
#' @usage sampleData(object, ...)
#' @export
NULL



#' @rdname show
#' @name show
#' @importFrom methods show
#' @usage show(object)
#' @export
NULL



#' @rdname topTables
#' @name topTables
#' @importFrom AcidGenerics topTables
#' @usage topTables(object, ...)
#' @export
NULL



#' @rdname transformType
#' @name transformType
#' @importFrom AcidGenerics transformType
#' @usage transformType(object, ...)
#' @export
NULL



#' @rdname updateObject
#' @name updateObject
#' @importFrom AcidGenerics updateObject
#' @usage updateObject(object, ..., verbose = FALSE)
#' @export
NULL
