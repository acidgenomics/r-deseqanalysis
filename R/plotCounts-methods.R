#' @name plotCounts
#' @inherit acidplots::plotCounts
#'
#' @inheritParams acidroxygen::params
#' @inheritParams params
#' @param ... Additional arguments.
#'
#' @param transform `logical(1)`.
#'   Visualize using `DESeqTransform` log2 variance-stabilized counts, rather
#'   than `DESeqDataSet` size-factor normalized counts.
#'
#' @examples
#' data(deseq)
#'
#' ## Get genes from working example.
#' res <- results(deseq, results = 1L)
#' genes <- head(rownames(res))
#' print(genes)
#'
#' ## DESeqAnalysis ====
#' plotCounts(deseq, genes = genes, style = "facet")
#' plotCounts(deseq, genes = genes, style = "wide")
NULL



#' @rdname plotCounts
#' @name plotCounts
#' @importFrom bioverbs plotCounts
#' @usage plotCounts(object, ...)
#' @export
NULL



## Note that DESeqDataSet is supported in basejump SummarizedExperiment method.
## That will detect the object and plot normalized counts automatically.
## Updated 2019-07-23.
`plotCounts,DESeqAnalysis` <-  # nolint
    function(object, genes, transform = FALSE) {
        validObject(object)
        assert(
            isCharacter(genes),
            isFlag(transform)
        )

        if (isTRUE(transform)) {
            object <- as(object, "DESeqTransform")
            if ("rlogIntercept" %in% colnames(mcols(object))) {
                countsAxisLabel <- "rlog counts (log2)"
            } else {
                countsAxisLabel <- "vst counts (log2)"
            }
        } else {
            object <- as(object, "DESeqDataSet")
            countsAxisLabel <- "normalized counts"
        }

        do.call(
            what = plotCounts,
            args = matchArgsToDoCall(
                args = list(
                    object = object,
                    genes = genes,
                    countsAxisLabel = countsAxisLabel
                ),
                removeFormals = "transform"
            )
        )
    }

f1 <- formals(`plotCounts,DESeqAnalysis`)
f2 <- methodFormals(
    f = "plotCounts",
    signature = "SummarizedExperiment",
    package = "acidplots"
)
f2 <- f2[setdiff(names(f2), c(names(f1), "assay", "countsAxisLabel"))]
f <- c(f1, f2)
formals(`plotCounts,DESeqAnalysis`) <- f



#' @rdname plotCounts
#' @export
setMethod(
    f = "plotCounts",
    signature = signature("DESeqAnalysis"),
    definition = `plotCounts,DESeqAnalysis`
)
