#' @name plotCounts
#' @inherit basejump::plotCounts
#' @inheritParams basejump::params
#' @inheritParams params
#' @examples
#' data(deseq)
#'
#' ## Get genes from DESeqDataSet.
#' dds <- as(deseq, "DESeqDataSet")
#' genes <- head(rownames(dds))
#' print(genes)
#'
#' ## DESeqAnalysis ====
#' plotCounts(deseq, genes = genes, style = "facet")
#' plotCounts(deseq, genes = genes, style = "wide")
NULL



#' @importFrom bioverbs plotCounts
#' @aliases NULL
#' @export
bioverbs::plotCounts



# Note that DESeqDataSet is supported in basejump SummarizedExperiment method.
# That will detect the object and plot normalized counts automatically.



plotCounts.DESeqAnalysis <-  # nolint
    function(object) {
        validObject(object)
        # Using DESeqTransform data here.
        dt <- slot(object, "transform")
        if ("rlogIntercept" %in% colnames(dt)) {
            countsAxisLabel <- "rlog counts (log2)"
        } else {
            countsAxisLabel <- "vst counts (log2)"
        }
        do.call(
            what = plotCounts,
            args = matchArgsToDoCall(
                args = list(
                    object = dt,
                    genes = genes,
                    countsAxisLabel = countsAxisLabel
                )
            )
        )
    }

f <- methodFormals(
    f = "plotCounts",
    signature = "SummarizedExperiment",
    package = "basejump"
)
f <- f[setdiff(names(f), c("assay", "countsAxisLabel"))]
formals(plotCounts.DESeqAnalysis) <- f



#' @rdname plotCounts
#' @export
setMethod(
    f = "plotCounts",
    signature = signature("DESeqAnalysis"),
    definition = plotCounts.DESeqAnalysis
)
