# Can use `priorInfo()` to inform the user about `lfcShrink` slot.
.matchResults <- function(
    object,
    results,
    lfcShrink = TRUE
) {
    if (missing(results)) {
        stop(paste(
            "Failed to match results.",
            "Required `results` argument is missing.",
            "Specify as either name or position scalar.",
            sep = "\n"
        ))
    }
    assert(
        is(object, "DESeqAnalysis"),
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
