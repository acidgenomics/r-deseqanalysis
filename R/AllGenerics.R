#' @rdname alphaSummary
#' @name alphaSummary
#' @usage alphaSummary(object, ...)
#' @importFrom bioverbs alphaSummary
#' @export
NULL



#' @rdname contrastName
#' @name contrastName
#' @importFrom bioverbs contrastName
#' @usage contrastName(object, ...)
#' @export
NULL



#' @rdname contrastSamples
#' @name contrastSamples
#' @importFrom bioverbs contrastSamples
#' @usage contrastSamples(object, ...)
#' @export
NULL



#' @rdname deg
#' @name deg
#' @importFrom bioverbs deg
#' @usage deg(object, ...)
#' @export
NULL



#' @rdname export
#' @name export
#' @importFrom bioverbs export
#' @usage export(x, ...)
#' @export
NULL



#' @rdname lfcShrinkType
#' @export
setGeneric(
    name = "lfcShrinkType",
    def = function(object, ...) {
        standardGeneric("lfcShrinkType")
    }
)



#' @rdname markdown
#' @name markdown
#' @importFrom bioverbs markdown
#' @usage markdown(object, ...)
#' @export
NULL



#' @rdname plotCounts
#' @name plotCounts
#' @importFrom bioverbs plotCounts
#' @usage plotCounts(object, ...)
#' @export
NULL



#' @rdname plotDEGHeatmap
#' @name plotDEGHeatmap
#' @importFrom bioverbs plotDEGHeatmap
#' @usage plotDEGHeatmap(object, ...)
#' @export
NULL



#' @rdname plotDEGPCA
#' @name plotDEGPCA
#' @importFrom bioverbs plotDEGPCA
#' @usage plotDEGPCA(object, ...)
#' @export
NULL



#' @rdname plotMA
#' @name plotMA
#' @importFrom BiocGenerics plotMA
#' @usage plotMA(object, ...)
#' @export
NULL



#' @rdname plotMA
#' @export
setGeneric(
    name = "plotMA2",
    def = function(object, ...) {
        standardGeneric("plotMA2")
    }
)



#' @rdname plotVolcano
#' @name plotVolcano
#' @importFrom bioverbs plotVolcano
#' @usage plotVolcano(object, ...)
#' @export
NULL



#' @rdname results
#' @name results
#' @importFrom bioverbs results
#' @usage results(object, ...)
#' @export
NULL



#' @rdname resultsMatrix
#' @name resultsMatrix
#' @importFrom bioverbs resultsMatrix
#' @usage resultsMatrix(object, ...)
#' @export
NULL



#' @rdname resultsNames
#' @name resultsNames
#' @importFrom bioverbs resultsNames
#' @usage resultsNames(object, ...)
#' @export
NULL



#' @rdname resultsTables
#' @name resultsTables
#' @importFrom bioverbs resultsTables
#' @usage resultsTables(object, ...)
#' @export
NULL



#' @name sampleData
#' @importFrom bioverbs sampleData
#' @inherit bioverbs::sampleData
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
#' @importFrom bioverbs topTables
#' @usage topTables(object, ...)
#' @export
NULL



#' @rdname transformType
#' @export
setGeneric(
    name = "transformType",
    def = function(object, ...) {
        standardGeneric("transformType")
    }
)



#' @rdname updateObject
#' @name updateObject
#' @importFrom BiocGenerics updateObject
#' @usage updateObject(object, ...)
#' @export
NULL
