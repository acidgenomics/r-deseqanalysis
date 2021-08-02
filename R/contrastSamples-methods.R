#' @name contrastSamples
#' @inherit AcidGenerics::contrastSamples
#' @note Updated 2021-08-02.
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
#' - `list`: Named list containing `"numerator"` and `"denominator"` of sample
#'   identifiers corresponding to column names of `DESeqDataSet`.
#' - `character`: Sample identifiers, corresponding to the column names
#'   of `DESeqDataSet`.
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



## Updated 2021-08-02.
`contrastSamples,DESeqAnalysis` <-  # nolint
    function(
        object,
        i,
        quiet = FALSE,
        return = c("list", "character")
    ) {
        validObject(object)
        assert(isFlag(quiet))
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
        match <- str_match(
            string = contrastSansFactor,
            pattern = "^(.+)_vs_(.+)$"
        )
        assert(
            identical(dim(match), c(1L, 3L)),
            msg = "Contrast name match failure."
        )
        numeratorCol <- match[1L, 2L]
        numerator <- samples[factor %in% numeratorCol]
        denominatorCol <- match[1L, 3L]
        denominator <- samples[factor %in% denominatorCol]
        assert(
            isSubset(numeratorCol, factor),
            hasLength(numerator),
            isSubset(denominatorCol, factor),
            hasLength(denominator),
            msg = "Failed to extract numerator/denominator samples."
        )
        if (isFALSE(quiet)) {
            dl(c(
                "Numerator samples" = toString(numerator, width = 200L),
                "Denominator samples" = toString(denominator, width = 200L)
            ))
        }
        switch(
            EXPR = return,
            "character" = {
                c(numerator, denominator)
            },
            "list" = {
                list(
                    "numerator" = numerator,
                    "denominator" = denominator
                )
            }
        )
    }



#' @rdname contrastSamples
#' @export
setMethod(
    f = "contrastSamples",
    signature = signature("DESeqAnalysis"),
    definition = `contrastSamples,DESeqAnalysis`
)
