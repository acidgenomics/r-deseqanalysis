## FIXME Add support for difference of differences.



#' @name contrastSamples
#' @inherit bioverbs::contrastSamples
#' @note Updated 2019-12-16.
#'
#' @inheritParams acidroxygen::params
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



#' @rdname contrastSamples
#' @name contrastSamples
#' @importFrom bioverbs contrastSamples
#' @usage contrastSamples(object, ...)
#' @export
NULL



## This has been split out to an internal function, so we can support
## interaction effect (difference of differences) contrasts more easily.
## Updated 2019-12-16.
.samplesFromContrast <- function(
    dds,
    contrast,
    factorCol
) {
    assert(
        is(dds, "DESeqDataSet"),
        isString(contrast),
        isString(factorCol)
    )
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
    message(sprintf(
        "Numerator samples: %s.",
        toString(numerator, width = 200L)
    ))
    ## Denominator.
    denominatorCol <- match[1L, 3L]
    assert(isSubset(denominatorCol, factor))
    denominator <- samples[factor %in% denominatorCol]
    assert(hasLength(denominator))
    message(sprintf(
        "Denominator samples: %s.",
        toString(denominator, width = 200L)
    ))
    ## Return.
    sort(c(numerator, denominator))
}



## Updated 2019-12-16.
`contrastSamples,DESeqAnalysis` <-  # nolint
    function(object, i, ...) {
        ## nocov start
        call <- match.call()
        ## results
        if ("results" %in% names(call)) {
            stop("'results' is defunct in favor of 'i'.")
        }
        assert(isSubset(
            x = setdiff(names(call), ""),
            y = names(formals())
        ))
        rm(call)
        ## nocov end
        validObject(object)
        suppressMessages(
            res <- results(object = object, i = i)
        )
        ## If we've defined a subset of samples for the contrast, stash them
        ## in DESeqResults metadata. Otherwise, there's no way to trace this
        ## back to a match in DESeqDataSet.
        samples <- metadata(res)[["samples"]]
        if (hasLength(samples)) {
            return(samples)
        }
        contrast <- contrastName(
            object = res,
            format = "resultsNames",
            useStash = FALSE
        )
        assert(
            isString(contrast),
            assert(grepl("_vs_", contrast))
        )
        message(sprintf("Contrast: %s", contrast))
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
        message(sprintf("Factor column: %s.", factorCol))





        ## Look for interaction effect (difference of differences).
        ## e.g. "group_B_vs_A_group_C_vs_A_effect".
        if (isTRUE(grepl(pattern = "_effect$", x = contrast))) {
            message("Interaction effect (difference of differences) detected.")
            interaction <- TRUE
            x <- contrast
            x <- sub("_effect$", "", x)
            loc <- str_locate_all(string = x, pattern = factorCol)[[1L]]
            contrast1 <- substr(x = x, start = loc[1,1], stop = loc[2,1] - 2L)
            contrast2 <- substr(x = x, start = loc[2,1], stop = nchar(x))
        } else {
            interaction <- FALSE
        }










        ## FIXME RETHINK THIS APPROACH.
        ## Remove the factor prefix from our contrast.

        ## FIXME Just include the factorCol








    }



#' @rdname contrastSamples
#' @export
setMethod(
    f = "contrastSamples",
    signature = signature("DESeqAnalysis"),
    definition = `contrastSamples,DESeqAnalysis`
)
