#' Principal component analysis plot
#'
#' @name plotPca
#' @note Updated 2022-09-27.
#'
#' @details Passes to `SummarizedExperiment` defined in AcidPlots package.
#'
#' @inheritParams AcidRoxygen::params
#'
#' @seealso
#' - `DESeq2::plotPCA`.
#'
#' @return `ggplot`.
#'
#' @examples
#' data(deseq)
#'
#' ## DESeqAnalysis ====
#' plotPca(deseq)
NULL



## Updated 2021-01-21.
`plotPca,DESeqAnalysis` <- # nolint
    function(object, ...) {
        assert(validObject(object))
        dt <- as(object, "DESeqTransform")
        rse <- as(dt, "RangedSummarizedExperiment")
        plotPca(rse, ...)
    }



#' @describeIn plotPca Extracts `DESeqTransform`, converts to
#' `RangedSummarizedExperiment`, and passes to method defined in AcidPlots.
#' @export
setMethod(
    f = "plotPca",
    signature = signature(object = "DESeqAnalysis"),
    definition = `plotPca,DESeqAnalysis`
)
