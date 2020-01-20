#' @name plotCounts
#' @inherit acidplots::plotCounts
#' @note Updated 2019-12-18.
#'
#' @inheritParams acidroxygen::params
#' @inheritParams params
#' @param transform `logical(1)`.
#'   Plot log2 variance-stabilized transform counts, defined internally in
#'   `DESeqTransform` object (see `transform` slot). If `FALSE`, plot the
#'   size factor adjusted counts from `DESeqDataSet`.
#' @param ... Additional arguments.
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



#' @rdname plotCounts
#' @name plotCounts
#' @importFrom acidgenerics plotCounts
#' @usage plotCounts(object, ...)
#' @export
NULL



## Updated 2019-11-18.
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
        labels[["countAxis"]] <- "norm counts"
        args[["labels"]] <- labels
        do.call(what = plotCounts, args = args)
    }



#' @describeIn plotCounts Plot size factor (i.e. library size) adjusted
#'   normalized counts. Arguments pass through to `SummarizedExperiment` method
#'   defined in acidplots package.
#' @export
setMethod(
    f = "plotCounts",
    signature = signature("DESeqDataSet"),
    definition = `plotCounts,DESeqDataSet`
)



## Updated 2019-11-18.
`plotCounts,DESeqTransform` <-  # nolint
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



#' @describeIn plotCounts Plot log2 variance-stabilized transformed counts.
#' @export
setMethod(
    f = "plotCounts",
    signature = signature("DESeqTransform"),
    definition = `plotCounts,DESeqTransform`
)



## Putting the args after `...` here so we can pass in genes easily as a
## positional argument, during interactive use.
## Updated 2019-12-18.
`plotCounts,DESeqAnalysis` <-  # nolint
    function(object, ..., samples = NULL, transform = FALSE) {
        validObject(object)
        assert(
            isCharacter(samples, nullOK = TRUE),
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



#' @describeIn plotCounts Plot either `DESeqDataSet` normalized counts or
#'   `DESeqTransform` log2 variance-stabilized counts.
#' @export
setMethod(
    f = "plotCounts",
    signature = signature("DESeqAnalysis"),
    definition = `plotCounts,DESeqAnalysis`
)
