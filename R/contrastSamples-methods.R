#' Match the Samples Used to Define Contrast
#'
#' Match the samples in a `DESeqDataSet` used to define contrast in a
#' corresponding `DESeqResults` object. Note that this only works for simple
#' (e.g. pairwise) contrasts and will intentionally error for more complex
#' comparisons.
#'
#' @note **EXPERIMENTAL METHOD**. This approach will error intentionally for
#'   `DESeqResults` objects generated with complex contrasts (e.g. interaction
#'   effect or LRT). It may be removed in a future release if this approach
#'   is too error-prone.
#'
#' @name contrastSamples
#' @inheritParams basejump::params
#' @inheritParams params
#'
#' @return `character`. Sample identifiers (corresponding to columns).
#'
#' @examples
#' data(deseq)
#'
#' ## DESeqAnalysis ====
#' contrastSamples(deseq, results = 1L)
NULL



contrastSamples.DESeqAnalysis <- function(object, results = 1L) {
    validObject(object)
    results <- .matchResults(object, results)
    contrast <- snake(contrastName(results))
    assert(grepl("_vs_", contrast))

    # Figure out which column was used to define the pairwise contrast.
    match <- str_match(contrast, "^([[:alnum:]]+)_(.+)_vs_(.+)$")
    factor <- match[1L, 2L]

    data <- as(object, "DESeqDataSet")
    samples <- colnames(data)

    colData <- colData(data)
    assert(hasRownames(colData))

    assert(isSubset(factor, colnames(colData)))
    message(paste("Factor column:", factor))
    factor <- snake(colData[[factor]])
    assert(is.factor(factor))

    numerator <- match[1L, 3L]
    assert(isSubset(numerator, factor))
    numerator <- samples[factor %in% numerator]
    message(paste("Numerator samples:", toString(numerator)))

    denominator <- match[1L, 4L]
    assert(isSubset(denominator, factor))
    denominator <- samples[factor %in% denominator]
    message(paste("Denominator samples:", toString(denominator)))

    c(numerator, denominator)
}



#' @rdname contrastSamples
#' @export
setMethod(
    f = "contrastSamples",
    signature = signature("DESeqAnalysis"),
    definition = contrastSamples.DESeqAnalysis
)
