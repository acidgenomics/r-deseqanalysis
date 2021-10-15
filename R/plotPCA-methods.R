#' @name plotPCA
#' @inherit AcidPlots::plotPCA
#' @note Updated 2021-03-15.
#'
#' @details Passes to `SummarizedExperiment` defined in AcidPlots package.
#'
#' @examples
#' data(deseq)
#'
#' ## DESeqAnalysis ====
#' plotPCA(deseq)
NULL



## Updated 2019-09-10.
`plotPCA,DESeqAnalysis` <-  # nolint
    function(object, ...) {
        validObject(object)
        dt <- as(object, "DESeqTransform")
        plotPCA(object = dt, ...)
    }



## Keep this here so we don't allow inheritance of SE method.
## Updated 2021-03-15.
`plotPCA,DESeqDataSet` <-  # nolint
    function(object, ...) {
        abort(sprintf(
            "Use {.cls %s} object instead of {.cls %s}.",
            "DESeqTransform", "DESeqDataSet"
        ))
    }



## Updated 2019-07-23.
`plotPCA,DESeqTransform` <-  # nolint
    function(object, ...) {
        rse <- as(object, "RangedSummarizedExperiment")
        plotPCA(rse, ...)
    }



#' @describeIn plotPCA Extracts `DESeqTransform` and passes to corresponding
#'   method.
#' @export
setMethod(
    f = "plotPCA",
    signature = signature(object = "DESeqAnalysis"),
    definition = `plotPCA,DESeqAnalysis`
)

#' @describeIn plotPCA Method intentionally errors. Use `DESeqAnalysis` or
#'   `DESeqTransform` methods instead.
#' @export
setMethod(
    f = "plotPCA",
    signature = signature(object = "DESeqDataSet"),
    definition = `plotPCA,DESeqDataSet`
)

#' @describeIn plotPCA Passes to `SummarizedExperiment` method defined in
#'   AcidPlots package. Uses values defined in
#'   [`assay()`][SummarizedExperiment::assay].
#' @export
setMethod(
    f = "plotPCA",
    signature = signature(object = "DESeqTransform"),
    definition = `plotPCA,DESeqTransform`
)
