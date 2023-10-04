#' @inherit DESeqAnalysis-class title description return
#' @export
#' @note Updated 2021-03-15.
#'
#' @param data `DESeqDataSet`.
#'
#' @param transform `DESeqTransform`.
#' `DESeq2::varianceStabilizingTransformation()` recommended by default.
#'
#' @param results `list` or single `DESeqResults`.
#' One or more unshrunken `DESeqResults`.
#' Assign the `DESeq2::results()` return here.
#'
#' @param lfcShrink `list`, single `DESeqResults`, or `NULL`.
#' *Optional*. One or more shrunken `DESeqResults`.
#' Assign the `DESeq2::lfcShrink()` return here.
#'
#' @examples
#' suppressPackageStartupMessages({
#'     library(S4Vectors)
#' })
#' data <- DESeq2::makeExampleDESeqDataSet()
#' rowRanges <- AcidGenomes::emptyRanges(names = rownames(data))
#' mcols(rowRanges)[["geneId"]] <- paste0("id", seq_len(length(rowRanges)))
#' mcols(rowRanges)[["geneName"]] <- paste0("name", seq_len(length(rowRanges)))
#' rowRanges(data) <- rowRanges
#' data <- DESeq2::DESeq(data)
#' class(data)
#'
#' transform <- DESeq2::varianceStabilizingTransformation(data)
#' class(transform)
#'
#' resultsNames(data)
#' name <- resultsNames(data)[[2L]]
#' results <- DESeq2::results(data, name = name)
#' class(results)
#'
#' lfcShrink <- DESeq2::lfcShrink(dds = data, res = results, coef = 2L)
#'
#' results <- list(results)
#' names(results) <- name
#'
#' lfcShrink <- list(lfcShrink)
#' names(lfcShrink) <- name
#'
#' object <- DESeqAnalysis(
#'     data = data,
#'     transform = transform,
#'     results = results,
#'     lfcShrink = lfcShrink
#' )
#' print(object)
DESeqAnalysis <- # nolint
    function(data,
             transform,
             results,
             lfcShrink = NULL) {
        ## Allow input of single `DESeqResults`.
        if (is(results, "DESeqResults")) {
            results <- .coerceResultsToList(results)
        }
        if (is(lfcShrink, "DESeqResults")) {
            lfcShrink <- .coerceResultsToList(lfcShrink)
        }
        ## Automatically convert `lfcShrink = NULL` to empty list.
        if (is.null(lfcShrink)) {
            lfcShrink <- list()
        }
        new(
            Class = "DESeqAnalysis",
            data = data,
            transform = transform,
            results = results,
            lfcShrink = lfcShrink,
            metadata = list("packageVersion" = .pkgVersion)
        )
    }
