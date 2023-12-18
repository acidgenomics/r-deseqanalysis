#' @name plotCounts
#' @inherit AcidPlots::plotCounts
#' @note Updated 2021-03-15.
#'
#' @inheritParams AcidRoxygen::params
#' @inheritParams params
#' @param ... Additional arguments.
#'
#' @param transform `logical(1)`.
#' Plot log2 variance-stabilized transform counts, defined internally in
#' `DESeqTransform` object (see `transform` slot). If `FALSE`, plot the
#' size factor adjusted counts from `DESeqDataSet`.
#'
#' @seealso `DESeq2::plotCounts`.
#'
#' @examples
#' data(deseq)
#'
#' ## Get genes from working example.
#' genes <- head(rownames(as(deseq, "DESeqDataSet")))
#' print(genes)
#'
#' ## DESeqAnalysis ====
#' plotCounts(deseq, genes = genes, style = "facet")
#' plotCounts(deseq, genes = genes, style = "wide")
NULL



## Putting the args after `...` here so we can pass in genes easily as a
## positional argument, during interactive use.
## Updated 2023-12-18.
`plotCounts,DESeqAnalysis` <- # nolint
    function(object, ..., samples = NULL, transform = FALSE) {
        assert(
            validObject(object),
            isCharacter(samples, nullOk = TRUE),
            isFlag(transform)
        )
        if (isTRUE(transform)) {
            class <- "DESeqTransform"
        } else {
            class <- "DESeqDataSet"
        }
        dots <- list(...)
        object <- as(object, Class = class)
        if (!is.null(samples)) {
            object <- object[, samples, drop = FALSE]
        }
        args <- c(object = object, dots)
        do.call(what = plotCounts, args = args)
    }



## Updated 2019-11-18.
`plotCounts,DESeqDataSet` <- # nolint
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
        labels[["countAxis"]] <- "norm counts"
        args[["labels"]] <- labels
        do.call(what = plotCounts, args = args)
    }



## Updated 2019-11-18.
`plotCounts,DESeqTransform` <- # nolint
    function(object, ...) {
        dots <- list(...)
        rse <- as(object, "RangedSummarizedExperiment")
        args <- c(object = rse, dots)
        labels <- args[["labels"]]
        if (is.null(labels)) {
            labels <- list()
        }
        type <- transformType(object)
        ## Abbreviate as acronym becuase this label it too long otherwise.
        if (identical(type, "varianceStabilizingTransformation")) {
            type <- "vst"
        }
        labels[["countAxis"]] <- paste0(type, " counts (log2)")
        args[["labels"]] <- labels
        do.call(what = plotCounts, args = args)
    }



#' @describeIn plotCounts Plot either `DESeqDataSet` normalized counts or
#' `DESeqTransform` log2 variance-stabilized counts.
#' @export
setMethod(
    f = "plotCounts",
    signature = signature(object = "DESeqAnalysis"),
    definition = `plotCounts,DESeqAnalysis`
)

#' @describeIn plotCounts Plot size factor (i.e. library size) adjusted
#' normalized counts. Arguments pass through to `SummarizedExperiment` method
#' defined in AcidPlots package.
#' @export
setMethod(
    f = "plotCounts",
    signature = signature(object = "DESeqDataSet"),
    definition = `plotCounts,DESeqDataSet`
)

#' @describeIn plotCounts Plot log2 variance-stabilized transformed counts.
#' @export
setMethod(
    f = "plotCounts",
    signature = signature(object = "DESeqTransform"),
    definition = `plotCounts,DESeqTransform`
)
