#' @name contrastSamples
#' @inherit bioverbs::contrastSamples
#'
#' @inheritParams basejump::params
#' @inheritParams params
#' @param ... Additional arguments.
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



#' @rdname contrastSamples
#' @name contrastSamples
#' @importFrom bioverbs contrastSamples
#' @usage contrastSamples(object, ...)
#' @export
NULL



## Updated 2019-07-23.
`contrastSamples,DESeqAnalysis` <-  # nolint
    function(object, results) {
        validObject(object)
        suppressMessages(
            results <- results(object = object, results = results)
        )

        ## If we've defined a subset of samples for the contrast, stash them
        ## in DESeqResults metadata. Otherwise, there's no way to trace this
        ## back to a match in DESeqDataSet.
        samples <- metadata(results)[["samples"]]
        if (hasLength(samples)) {
            return(samples)
        }

        contrast <- contrastName(results, format = "resultsNames")
        assert(
            isString(contrast),
            assert(grepl("_vs_", contrast))
        )

        data <- as(object, "DESeqDataSet")
        samples <- colnames(data)
        colData <- colData(data)
        assert(hasRownames(colData))

        ## Inform if the contrast doesn't exist in DESeqDataSet resultsNames.
        ## Note that this can happen for complex contrasts, so don't warn.
        resultsNames <- resultsNames(data)
        if (!contrast %in% resultsNames) {
            message(paste0(
                "Note: ", contrast, " is not defined in resultsNames.\n",
                "This can happen with complex contrasts, ",
                "and is generally safe to ignore."
            ))
        }

        ## Loop across the colData column names and determine which column
        ## matches the prefix of the defined contrast.
        match <- vapply(
            X = colnames(colData),
            FUN = function(col) {
                any(grepl(pattern = paste0("^", col), x = contrast))
            },
            FUN.VALUE = logical(1L),
            USE.NAMES = TRUE
        )
        assert(hasLength(sum(match), n = 1L))
        factorCol <- names(match)[match]
        message(paste("Factor column:", factorCol))
        factor <- colData[[factorCol]]
        assert(is.factor(factor))

        ## Now remove the factor prefix from our contrast.
        contrastSansFactor <- sub(
            pattern = paste0("^", factorCol, "_"),
            replacement = "",
            x = contrast
        )
        match <- str_match(
            string = contrastSansFactor,
            pattern = "^(.+)_vs_(.+)$"
        )

        numeratorCol <- match[1L, 2L]
        assert(isSubset(numeratorCol, factor))
        numerator <- samples[factor %in% numeratorCol]
        assert(hasLength(numerator))
        message(paste("Numerator samples:", toString(numerator)))

        denominatorCol <- match[1L, 3L]
        assert(isSubset(denominatorCol, factor))
        denominator <- samples[factor %in% denominatorCol]
        assert(hasLength(denominator))
        message(paste("Denominator samples:", toString(denominator)))

        c(numerator, denominator)
    }



#' @rdname contrastSamples
#' @export
setMethod(
    f = "contrastSamples",
    signature = signature("DESeqAnalysis"),
    definition = `contrastSamples,DESeqAnalysis`
)
