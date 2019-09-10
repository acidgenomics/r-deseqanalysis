#' @name results
#' @inherit bioverbs::results
#' @note Updated 2019-09-10.
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



## Updated 2019-09-10.
`results,DESeqAnalysis` <-  # nolint
    function(object, results, lfcShrink = FALSE) {
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
        name <- resultsNames(object)[[results]]
        data <- resultsList[[results]]
        assert(is(data, "DESeqResults"))
        if (isTRUE(lfcShrink)) {
            msg <- paste(name, "(shrunken LFC)")
        } else {
            msg <- paste(name, "(unshrunken LFC)")
        }
        message(msg)
        data
    }



#' @rdname results
#' @export
setMethod(
    f = "results",
    signature = signature("DESeqAnalysis"),
    definition = `results,DESeqAnalysis`
)
