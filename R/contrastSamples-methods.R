#' @name contrastSamples
#' @inherit bioverbs::contrastSamples
#' @inheritParams basejump::params
#' @inheritParams params
#'
#' @details
#' Match the samples in a `DESeqDataSet` used to define contrast in a
#' corresponding `DESeqResults` object. Note that this only works for simple
#' (e.g. pairwise) contrasts and will intentionally error for more complex
#' comparisons.
#'
#' @note **EXPERIMENTAL**. This approach will error intentionally for
#'   `DESeqResults` objects generated with complex contrasts (e.g. interaction
#'   effect or LRT). It may be removed in a future release if this approach is
#'   too error-prone.
#'
#' @examples
#' data(deseq)
#'
#' ## DESeqAnalysis ====
#' contrastSamples(deseq, results = 1L)
NULL



#' @importFrom bioverbs contrastSamples
#' @aliases NULL
#' @export
bioverbs::contrastSamples



contrastSamples.DESeqAnalysis <-  # nolint
    function(object, results) {
        validObject(object)
        results <- .matchResults(object, results)
        contrast <- makeNames(contrastName(results))
        assert(grepl("_vs_", contrast))

        # Inform if the contrast doesn't exist in DESeqDataSet resultsNames.
        # Note that this can happen for complex contrasts, so don't warn.
        resultsNames <- resultsNames(object@data)
        if (!contrast %in% resultsNames) {
            message(paste0(
                "Note: ", contrast, " not defined in resultsNames.\n",
                "This can happen with complex contrasts ",
                "and is safe to ignore.\n",
                printString(resultsNames)
            ))
        }

        # Figure out which column was used to define the pairwise contrast.
        match <- str_match(contrast, "^([[:alnum:]]+)_(.+)_vs_(.+)$")
        factor <- match[1L, 2L]

        data <- as(object, "DESeqDataSet")
        samples <- colnames(data)

        colData <- colData(data)
        assert(hasRownames(colData))

        assert(isSubset(factor, colnames(colData)))
        message(paste("Factor column:", factor))
        factor <- colData[[factor]]
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
