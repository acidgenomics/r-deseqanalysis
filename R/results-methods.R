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
#' x <- results(deseq, i = 2L, lfcShrink = FALSE)
#' class(x)
#' colnames(x)
#'
#' x <- results(deseq, i = 1L, lfcShrink = TRUE)
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



## Updated 2020-08-04.
`results,DESeqAnalysis` <-  # nolint
    function(
        object,
        i,
        lfcShrink = NULL,
        ...
    ) {
        if (is.null(lfcShrink)) {
            lfcShrink <- lfcShrink(object)
        }
        assert(
            is(object, "DESeqAnalysis"),
            isScalar(i),
            isFlag(lfcShrink)
        )
        if (isCharacter(i)) {
            assert(isSubset(i, resultsNames(object)))
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
        data <- resultsList[[i]]
        assert(is(data, "DESeqResults"))
        ## Slot the contrast name into DESeqResults metadata.
        name <- contrastName(object, i = i)
        contrastName(data) <- name
        if (isTRUE(lfcShrink)) {
            msg <- paste(name, "(shrunken LFC)")
        } else {
            msg <- paste(name, "(unshrunken LFC)")
        }
        message(msg)
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
