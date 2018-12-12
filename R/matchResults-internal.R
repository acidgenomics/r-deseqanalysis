# Can use `priorInfo` to inform the user about `lfcShrink`.
.matchResults <- function(
    object,
    results,
    lfcShrink = TRUE
) {
    assert(is(object, "DESeqAnalysis"))
    # Default to using the first contrast, for convenience.
    if (missing(results)) {
        results <- 1L
    }
    assert(
        isScalar(results),
        isFlag(lfcShrink)
    )
    # Match shrunken results by default.
    if (
        isTRUE(lfcShrink) &&
        length(object@lfcShrink) > 0L
    ) {
        slotName <- "lfcShrink"
    } else {
        slotName <- "results"
    }
    results <- slot(object, name = slotName)[[results]]
    assert(is(results, "DESeqResults"))

    # Inform the user about which data we're using.
    msg <- paste("DESeqResults:", contrastName(results))
    if (isTRUE(lfcShrink)) {
        msg <- paste(msg, "(shrunken LFC)")
    }
    message(msg)

    results
}
