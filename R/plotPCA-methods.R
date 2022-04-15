#' Principal component analysis plot
#'
#' @name plotPCA
#' @note Updated 2022-03-08.
#'
#' @details Passes to `SummarizedExperiment` defined in AcidPlots package.
#'
#' @inheritParams AcidRoxygen::params
#'
#' @return `ggplot`.
#'
#' @examples
#' data(deseq)
#'
#' ## DESeqAnalysis ====
#' plotPCA(deseq)
NULL



## Updated 2021-01-21.
`plotPCA,DESeqAnalysis` <- # nolint
    function(object, ...) {
        validObject(object)
        dt <- as(object, "DESeqTransform")
        rse <- as(dt, "RangedSummarizedExperiment")
        plotPCA(rse, ...)
    }



#' @describeIn plotPCA Extracts `DESeqTransform`, converts to
#' `RangedSummarizedExperiment`, and passes to method defined in AcidPlots.
#' @export
setMethod(
    f = "plotPCA",
    signature = signature(object = "DESeqAnalysis"),
    definition = `plotPCA,DESeqAnalysis`
)
