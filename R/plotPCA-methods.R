#' @name plotPCA
#' @inherit AcidPlots::plotPCA
#' @note Updated 2020-08-04.
#'
#' @details Passes to `SummarizedExperiment` defined in AcidPlots package.
#'
#' @examples
#' data(deseq)
#'
#' ## DESeqAnalysis ====
#' plotPCA(deseq)
NULL



## Updated 2020-07-28.
`plotPCA,DESeqDataSet` <-  # nolint
    function(object, ...) {
        stop("Use DESeqTransform object instead of DESeqDataSet.")
    }



#' @rdname plotPCA
#' @export
setMethod(
    f = "plotPCA",
    signature = signature("DESeqDataSet"),
    definition = `plotPCA,DESeqDataSet`
)



## Updated 2019-07-23.
`plotPCA,DESeqTransform` <-  # nolint
    getMethod(
        f = "plotPCA",
        signature = "SummarizedExperiment",
        where = "AcidPlots"
    )



#' @describeIn plotPCA Passes to `SummarizedExperiment` method defined in
#'   AcidPlots package. Uses values defined in
#'   [`assay()`][SummarizedExperiment::assay].
#' @export
setMethod(
    f = "plotPCA",
    signature = signature("DESeqTransform"),
    definition = `plotPCA,DESeqTransform`
)



## Updated 2019-09-10.
`plotPCA,DESeqAnalysis` <-  # nolint
    function(object, ...) {
        validObject(object)
        dt <- as(object, "DESeqTransform")
        plotPCA(object = dt, ...)
    }



#' @describeIn plotPCA Extracts `DESeqTransform` and passes to corresponding
#'   method.
#' @export
setMethod(
    f = "plotPCA",
    signature = signature("DESeqAnalysis"),
    definition = `plotPCA,DESeqAnalysis`
)
