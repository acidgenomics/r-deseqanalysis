#' @name plotCounts
#' @inherit acidplots::plotCounts
#' @note Updated 2019-09-17.
#'
#' @inheritParams acidroxygen::params
#' @inheritParams params
#' @param ... Additional arguments.
#'
#' @examples
#' data(deseq)
#'
#' ## Get genes from working example.
#' res <- results(deseq, i = 1L)
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



## Updated 2019-09-17.
`plotCounts,DESeqDataSet` <-  # nolint
    function(object, ...) {
        dots <- list(...)
        rse <- as(object, "RangedSummarizedExperiment")
        assays(rse) <-
            SimpleList(normalized = counts(object, normalized = TRUE))
        args <- c(object = rse, dots)
        labels <- args[["labels"]]
        if (is.null(labels)) {
            labels <- list()
        }
        labels[["countAxis"]] <- "normalized counts"
        args[["labels"]] <- labels
        do.call(what = plotCounts, args = args)
    }



#' @describeIn plotCounts Automatically plots size factor (i.e. library size)
#'   normalized counts. Arguments pass through to `SummarizedExperiment` method
#'   defined in acidplots package.
#' @export
setMethod(
    f = "plotCounts",
    signature = signature("DESeqDataSet"),
    definition = `plotCounts,DESeqDataSet`
)



## Updated 2019-09-17.
`plotCounts,DESeqAnalysis` <-  # nolint
    function(object, ...) {
        validObject(object)
        plotCounts(object = as(object, "DESeqDataSet"), ...)
    }



#' @describeIn plotCounts Passes to `DESeqDataSet` method.
#' @export
setMethod(
    f = "plotCounts",
    signature = signature("DESeqAnalysis"),
    definition = `plotCounts,DESeqAnalysis`
)
