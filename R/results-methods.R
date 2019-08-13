#' @name results
#' @inherit bioverbs::results
#' @note Updated 2019-07-30.
#'
#' @inheritParams acidroxygen::params
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



## Updated 2019-07-23.
`results,DESeqDataSet` <-  # nolint
    function(object, ...) {
        DESeq2::results(object, ...)
    }



#' @rdname results
#' @export
setMethod(
    f = "results",
    signature = signature("DESeqDataSet"),
    definition = `results,DESeqDataSet`
)



## Updated 2019-07-30.
`results,DESeqAnalysis` <-  # nolint
    function(object, results, lfcShrink = FALSE) {
        if (missing(results)) {
            stop(
                "Failed to match results.\n",
                "Required 'results' argument is missing.\n",
                "Specify as either name or position scalar."
            )
        }
        assert(
            is(object, "DESeqAnalysis"),
            isScalar(results),
            isFlag(lfcShrink)
        )
        if (isCharacter(results)) {
            assert(isSubset(results, resultsNames(object)))
        }

        ## Match the results.
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
            stop(
                "Shrunken LFC values were requested, ",
                "but object does not contain DESeqResults ",
                "defined in 'lfcShrink' slot.\n",
                "Set 'lfcShrink = FALSE'."
            )
        }

        resultsList <- slot(object, name = slotName)
        results <- resultsList[[results]]
        assert(is(results, "DESeqResults"))

        ## Inform the user about which data we're using.
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
    definition = `results,DESeqAnalysis`
)
