#' @name contrastSamples
#' @inherit AcidGenerics::contrastSamples
#' @note Updated 2021-03-15.
#'
#' @inheritParams AcidRoxygen::params
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
#' contrastSamples(deseq, i = 1L)
NULL



## This has been split out to an internal function, so we can support
## interaction effect (difference of differences) contrasts more easily.
## Updated 2021-03-15.
.contrastSamples <- function(
    dds,
    contrast,
    factorCol,
    quiet = FALSE
) {
    assert(
        is(dds, "DESeqDataSet"),
        isString(contrast),
        isString(factorCol),
        isFlag(quiet)
    )
    colData <- colData(dds)
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
    ## Numerator.
    numeratorCol <- match[1L, 2L]
    assert(isSubset(numeratorCol, factor))
    numerator <- samples[factor %in% numeratorCol]
    assert(hasLength(numerator))
    if (isFALSE(quiet)) {
        dl(c("Numerator samples" = toString(numerator, width = 200L)))
    }
    ## Denominator.
    denominatorCol <- match[1L, 3L]
    assert(isSubset(denominatorCol, factor))
    denominator <- samples[factor %in% denominatorCol]
    assert(hasLength(denominator))
    if (isFALSE(quiet)) {
        dl(c("Denominator samples" = toString(denominator, width = 200L)))
    }
    sort(c(numerator, denominator))
}



## Updated 2021-03-15.
`contrastSamples,DESeqAnalysis` <-  # nolint
    function(object, i, quiet = FALSE) {
        validObject(object)
        assert(isFlag(quiet))
        res <- results(object, i = i, quiet = TRUE)
        ## If we've defined a subset of samples for the contrast, stash them
        ## in DESeqResults metadata. Otherwise, there's no way to trace this
        ## back to a match in DESeqDataSet.
        samples <- metadata(res)[["samples"]]
        if (hasLength(samples)) {
            return(samples)
        }
        ## This step may error if we use a metadata stash, which is uncommon.
        contrast <- contrastName(res, format = "resultsNames")
        assert(
            isString(contrast),
            assert(grepl("_vs_", contrast))
        )
        if (isFALSE(quiet)) {
            dl(c("Contrast" = contrast))
        }
        dds <- as(object, "DESeqDataSet")
        colData <- colData(dds)
        assert(hasRownames(colData))
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
        assert(identical(sum(match), 1L))
        factorCol <- names(match)[match]
        if (isFALSE(quiet)) {
            dl(c("Factor column" = factorCol))
        }
        ## Look for interaction effect (difference of differences).
        ## e.g. "group_B_vs_A_group_C_vs_A_effect".
        if (isTRUE(grepl(pattern = "_effect$", x = contrast))) {
            if (isFALSE(quiet)) {
                alertInfo(
                    "Interaction effect (difference of differences) detected."
                )
            }
            interaction <- TRUE
            x <- contrast
            x <- sub("_effect$", "", x)
            loc <- str_locate_all(string = x, pattern = factorCol)[[1L]]
            contrast1 <-
                substr(x = x, start = loc[1L, 1L], stop = loc[2L, 1L] - 2L)
            contrast2 <-
                substr(x = x, start = loc[2L, 1L], stop = nchar(x))
            if (isFALSE(quiet)) {
                dl(c("Contrast 1" = contrast1))
            }
            samples1 <- .contrastSamples(
                dds = dds,
                contrast = contrast1,
                factorCol = factorCol,
                quiet = quiet
            )
            if (isFALSE(quiet)) {
                dl(c("Contrast 2" = contrast2))
            }
            samples2 <- .contrastSamples(
                dds = dds,
                contrast = contrast2,
                factorCol = factorCol,
                quiet = quiet
            )
            samples <- unique(c(samples1, samples2))
        } else {
            interaction <- FALSE
            samples <- .contrastSamples(
                dds = dds,
                contrast = contrast,
                factorCol = factorCol,
                quiet = quiet
            )
        }
        samples
    }



#' @rdname contrastSamples
#' @export
setMethod(
    f = "contrastSamples",
    signature = signature("DESeqAnalysis"),
    definition = `contrastSamples,DESeqAnalysis`
)
