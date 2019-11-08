#' Results
#'
#' @name results
#' @note Updated 2019-11-08.
#'
#' @inheritParams acidroxygen::params
#' @inheritParams params
#' @param ... Additional arguments.
#'
#' @examples
#' data(deseq)
#' x <- results(deseq, i = 1L)
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



## Updated 2019-11-08.
`results,DESeqAnalysis` <-  # nolint
    function(object, i, lfcShrink = FALSE, ...) {
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
