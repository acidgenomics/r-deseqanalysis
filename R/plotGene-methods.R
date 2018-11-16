#' @name plotGene
#' @inherit basejump::plotGene
#' @examples
#' data(deseq)
#' object <- deseq
#' 
#' g2s <- Gene2Symbol(as(object, "DESeqDataSet"))
#' geneIDs <- head(g2s[["geneID"]])
#' print(geneIDs)
#' geneNames <- head(g2s[["geneName"]])
#' print(geneNames)
#' 
#' plotGene(object, genes = geneIDs, style = "facet")
#' plotGene(object, genes = geneNames, style = "wide")
NULL



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
