#' @name contrastSamples
#' @inherit AcidGenerics::contrastSamples
#' @note Updated 2023-09-26.
#'
#' @details
#' Match the samples in a `DESeqDataSet` used to define contrast in a
#' corresponding `DESeqResults` object. Note that this only works for simple
#' (e.g. pairwise) contrasts and will intentionally error for more complex
#' comparisons.
#'
#' @inheritParams AcidRoxygen::params
#' @inheritParams params
#' @param ... Additional arguments.
#'
#' @return
#' - `character`: Sample identifiers, corresponding to the column names
#' of `DESeqDataSet`.
#' - `list`: Named list containing `"contrast"` and `"samples"` elements:
#' (1) `"contrast"`: `character` vector containing metadata on `"factor"`,
#' `"numerator"`, and `"denominator"` contrast elements;
#' (2) `"samples"`: ``list` containing `"numerator"` and `"denominator"` of
#' sample identifiers corresponding to column names of `DESeqDataSet`.
#'
#' @examples
#' data(deseq)
#'
#' ## DESeqAnalysis ====
#' x <- contrastSamples(deseq, i = 1L, return = "list")
#' print(x)
#'
#' x <- contrastSamples(deseq, i = 1L, return = "character")
#' print(x)
NULL



## Updated 2023-12-18.
`contrastSamples,DESeqAnalysis` <- # nolint
    function(object,
             i,
             quiet = FALSE,
             return = c("character", "list")) {
        assert(
            validObject(object),
            isFlag(quiet)
        )
        return <- match.arg(return)
        ## This will return "contrastName" stash in metadata.
        res <- results(object, i = i, quiet = TRUE)
        ## This will pull from metadata stash, for speed (see above).
        contrast <- contrastName(res, format = "resultsNames")
        assert(
            isString(contrast),
            isMatchingFixed(pattern = "_vs_", x = contrast),
            msg = "Invalid contrast name."
        )
        if (isFALSE(quiet)) {
            dl(c("Contrast" = contrast))
        }
        dds <- as(object, "DESeqDataSet")
        colData <- colData(dds)
        assert(
            hasColnames(dds),
            hasRownames(colData),
            hasColnames(colData)
        )
        ## Loop across the colData column names and determine which column
        ## matches the prefix of the defined contrast.
        match <- which(vapply(
            X = colnames(colData),
            FUN = function(col) {
                any(grepl(pattern = paste0("^", col, "_"), x = contrast))
            },
            FUN.VALUE = logical(1L),
            USE.NAMES = TRUE
        ))
        assert(
            isScalar(match),
            msg = "Failed to match contrast grouping column."
        )
        factorCol <- colnames(colData)[match]
        if (isFALSE(quiet)) {
            dl(c("Factor column" = factorCol))
        }
        factor <- colData[[factorCol]]
        assert(is.factor(factor))
        samples <- colnames(dds)
        contrastSansFactor <- sub(
            pattern = paste0("^", factorCol, "_"),
            replacement = "",
            x = contrast
        )
        match <- strMatch(
            x = contrastSansFactor,
            pattern = "^(.+)_vs_(.+)$"
        )
        assert(
            identical(dim(match), c(1L, 3L)),
            msg = "Contrast name match failure."
        )
        numeratorCol <- match[1L, 2L]
        numeratorSamples <- samples[factor %in% numeratorCol]
        denominatorCol <- match[1L, 3L]
        denominatorSamples <- samples[factor %in% denominatorCol]
        assert(
            isSubset(numeratorCol, factor),
            hasLength(numeratorSamples),
            isSubset(denominatorCol, factor),
            hasLength(denominatorSamples),
            msg = "Failed to extract numerator/denominator samples."
        )
        if (isFALSE(quiet)) {
            dl(c(
                "Numerator samples" =
                    toInlineString(numeratorSamples, n = 5L),
                "Denominator samples" =
                    toInlineString(denominatorSamples, n = 5L)
            ))
        }
        switch(
            EXPR = return,
            "character" = {
                c(numeratorSamples, denominatorSamples)
            },
            "list" = {
                list(
                    "contrast" = c(
                        "factor" = factorCol,
                        "numerator" = numeratorCol,
                        "denominator" = denominatorCol
                    ),
                    "samples" = list(
                        "numerator" = numeratorSamples,
                        "denominator" = denominatorSamples
                    )
                )
            }
        )
    }



#' @rdname contrastSamples
#' @export
setMethod(
    f = "contrastSamples",
    signature = signature(object = "DESeqAnalysis"),
    definition = `contrastSamples,DESeqAnalysis`
)
