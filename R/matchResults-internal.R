# Can use `priorInfo()` to inform the user about `lfcShrink()`.
.matchResults <- function(
    object,
    results,
    lfcShrink = TRUE
) {
    assert_is_all_of(object, "DESeqAnalysis")
    # Default to using the first contrast, for convenience.
    if (missing(results)) {
        results <- 1L
    }
    assert_is_scalar(results)
    assert_is_a_bool(lfcShrink)
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
    assert_is_all_of(results, "DESeqResults")

    # Inform the user about which data we're using.
    msg <- paste("DESeqResults:", contrastName(results))
    if (isTRUE(lfcShrink)) {
        msg <- paste(msg, "(shrunken LFC)")
    }
    message(msg)

    results
}
