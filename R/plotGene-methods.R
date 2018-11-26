#' @name plotGene
#' @inherit basejump::plotGene
#' @examples
#' data(deseq)
#'
#' ## Get genes from DESeqDataSet.
#' dds <- as(deseq, "DESeqDataSet")
#' genes <- head(rownames(dds))
#' print(genes)
#'
#' ## DESeqAnalysis ====
#' plotGene(deseq, genes = genes, style = "facet")
#' plotGene(deseq, genes = genes, style = "wide")
NULL



#' @importFrom basejump plotGene
#' @aliases NULL
#' @export
basejump::plotGene



plotGene.DESeqAnalysis <-  # nolint
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
            what = plotGene,
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
    f = "plotGene",
    signature = "SummarizedExperiment",
    package = "basejump"
)
f <- f[setdiff(names(f), c("assay", "countsAxisLabel"))]
formals(plotGene.DESeqAnalysis) <- f



#' @rdname plotGene
#' @export
setMethod(
    f = "plotGene",
    signature = signature("DESeqAnalysis"),
    definition = plotGene.DESeqAnalysis
)
