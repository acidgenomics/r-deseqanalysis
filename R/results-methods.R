#' @name results
#' @inherit bioverbs::results
#'
#' @inheritParams basejump::params
#' @inheritParams params
#' @param ... Additional arguments.
#'
#' @examples
#' data(deseq)
#' x <- results(deseq, results = 1L)
#' class(x)
NULL



#' @rdname results
#' @name results
#' @importFrom bioverbs results
#' @usage results(object, ...)
#' @export
NULL



results.DESeqDataSet <-  # nolint
    function(object, ...) {
        DESeq2::results(object, ...)
    }



#' @rdname results
#' @export
setMethod(
    f = "results",
    signature = signature("DESeqDataSet"),
    definition = results.DESeqDataSet
)



results.DESeqAnalysis <-  # nolint
    function(object, results, lfcShrink = FALSE) {
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

        # Match the results.
        if (identical(lfcShrink, FALSE)) {
            slotName <- "results"
        } else if (
            isTRUE(lfcShrink) &&
            hasLength(object@lfcShrink)
        ) {
            slotName <- "lfcShrink"
        } else if (
            isTRUE(lfcShrink) &&
            !hasLength(object@lfcShrink)
        ) {
            stop(paste(
                "Shrunken LFC values were requested,",
                "but object does not contain DESeqResults",
                "defined in `lfcShrink` slot.",
                "Set `lfcShrink = FALSE`."
            ))
        }

        results <- slot(object, name = slotName)[[results]]
        assert(is(results, "DESeqResults"))

        # Inform the user about which data we're using.
        msg <- contrastName(results)
        if (isTRUE(lfcShrink)) {
            msg <- paste(msg, "(shrunken LFC)")
        } else {
            msg <- paste(msg, "(unshrunken LFC)")
        }
        message(msg)

        results
    }



#' @rdname results
#' @export
setMethod(
    f = "results",
    signature = signature("DESeqAnalysis"),
    definition = results.DESeqAnalysis
)
