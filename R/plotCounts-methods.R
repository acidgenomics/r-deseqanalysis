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



# Note that we need to remap "object" to "dds" here.
plotCounts.DESeqDataSet <- function(object, ...) {
    do.call(
        what = DESeq2::plotCounts,
        args = matchArgsToDoCall(
            args = list(dds = object),
            removeFormals = "object"
        )
    )
}

f <- formals(DESeq2::plotCounts)
names(f)[[1L]] <- "object"
formals(plotCounts.DESeqDataSet) <- f



#' @rdname plotCounts
#' @export
setMethod(
    f = "plotCounts",
    signature = signature("DESeqDataSet"),
    definition = plotCounts.DESeqDataSet
)



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
