#' @name plotPCA
#' @inherit AcidPlots::plotPCA
#' @note Updated 2022-01-21.
#'
#' @details Passes to `SummarizedExperiment` defined in AcidPlots package.
#'
#' @examples
#' data(deseq)
#'
#' ## DESeqAnalysis ====
#' plotPCA(deseq)
NULL



## Updated 2021-01-21.
`plotPCA,DESeqAnalysis` <-  # nolint
    function(object, ...) {
        validObject(object)
        dt <- as(object, "DESeqTransform")
        rse <- as(dt, "RangedSummarizedExperiment")
        plotPCA(rse, ...)
    }



#' @describeIn plotPCA Extracts `DESeqTransform`, converts to
#'   `RangedSummarizedExperiment`, and passes to method defined in AcidPlots.
#' @export
setMethod(
    f = "plotPCA",
    signature = signature(object = "DESeqAnalysis"),
    definition = `plotPCA,DESeqAnalysis`
)
