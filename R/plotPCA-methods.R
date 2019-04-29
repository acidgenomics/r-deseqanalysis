#' @name plotPCA
#' @inherit acidplots::plotPCA
NULL



plotPCA.DESeqTransform <-  # nolint
    getMethod(
        f = "plotPCA",
        signature = "SummarizedExperiment",
        where = "acidplots"
    )



#' @rdname plotPCA
#' @export
setMethod(
    f = "plotPCA",
    signature = signature("DESeqTransform"),
    definition = plotPCA.DESeqTransform
)
