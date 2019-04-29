#' @name plotPCA
#' @inherit acidplots::plotPCA
#' @examples
#' data(deseq)
#'
#' ## DESeqAnalysis
#' plotPCA(deseq)
#'
#' ## DESeqTransform
#' dt <- as(deseq, "DESeqTransform")
#' plotPCA(dt)
NULL



#' @rdname plotPCA
#' @name plotPCA
#' @importFrom BiocGenerics plotPCA
#' @usage plotPCA(object, ...)
#' @export
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



plotPCA.DESeqAnalysis <-  # nolint
    function(object, ...) {
        validObject(object)
        message("Using DESeqTransform counts.")
        dt <- as(object, "DESeqTransform")
        plotPCA(object = dt, ...)
    }



#' @rdname plotPCA
#' @export
setMethod(
    f = "plotPCA",
    signature = signature("DESeqAnalysis"),
    definition = plotPCA.DESeqAnalysis
)
