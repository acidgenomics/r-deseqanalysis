#' Results
#'
#' @name results
#' @note Updated 2020-08-04.
#'
#' @inheritParams acidroxygen::params
#' @inheritParams params
#' @param ... Additional arguments.
#'
#' @return `DESeqResults`.
#'
#' @examples
#' data(deseq)
#'
#' ## DESeqAnalysis ====
#' x <- results(deseq, i = 1L)
#' class(x)
#' colnames(x)
NULL



#' @rdname results
#' @name results
#' @importFrom acidgenerics results
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



## Updated 2020-08-05.
`results,DESeqAnalysis` <-  # nolint
    function(object, i) {
        validObject(object)
        assert(isScalar(i))
        if (isCharacter(i)) {
            assert(isSubset(i, resultsNames(object)))
        }
        lfcShrink <- lfcShrink(object)
        if (identical(lfcShrink, FALSE)) {
            slotName <- "results"
        } else if (
            isTRUE(lfcShrink) &&
            hasLength(slot(object, name = "lfcShrink"))
        ) {
            slotName <- "lfcShrink"
        } else if (
            isTRUE(lfcShrink) &&
            !hasLength(slot(object, name = "lfcShrink"))
        ) {
            stop(
                "Shrunken LFC values were requested, ",
                "but object does not contain DESeqResults ",
                "defined in 'lfcShrink' slot.\n",
                "Set 'lfcShrink(object) <- FALSE'."
            )
        }
        resultsList <- slot(object, name = slotName)
        data <- resultsList[[i]]
        assert(is(data, "DESeqResults"))
        name <- contrastName(object, i = i)
        contrastName(data) <- name
        msg <- name
        if (isTRUE(lfcShrink)) {
            msg <- paste(msg, "(shrunken LFC)")
        }
        cli_alert_info(msg)
        validObject(data)
        data
    }



#' @rdname results
#' @export
setMethod(
    f = "results",
    signature = signature("DESeqAnalysis"),
    definition = `results,DESeqAnalysis`
)
