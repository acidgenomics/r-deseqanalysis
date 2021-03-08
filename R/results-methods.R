#' Results
#'
#' @name results
#' @note Updated 2021-03-08.
#'
#' @section Extra mode:
#'
#' Get the `DESeqDataSet`, and humanize the sample names. Note that we're not
#' calling `humanize()` here on the `DESeqDataSet`, because we want to keep the
#' gene identifiers in the row names. Use human-friendly sample names, defined
#' by the `sampleName` column in `colData`. We're using this downstream when
#' joining the normalized counts.
#'
#' @inheritParams AcidRoxygen::params
#' @inheritParams params
#' @param extra `logical(1)`.
#'   Include row data (i.e. gene metadata) and normalized counts from the
#'   internal `DESeqDataSet`.
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



## Updated 2021-03-08.
`results,DESeqAnalysis` <-  # nolint
    function(object, i, extra = FALSE) {
        validObject(object)
        assert(
            isScalar(i),
            isFlag(extra)
        )
        if (isCharacter(i)) {
            assert(isSubset(i, resultsNames(object)))
        }
        lfcShrink <- lfcShrink(object)
        if (!isTRUE(lfcShrink)) {
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
                "Set 'lfcShrink(object) <- NULL'."
            )
        }
        resList <- slot(object, name = slotName)
        res <- resList[[i]]
        assert(is(res, "DESeqResults"))
        if (isTRUE(extra)) {
            dds <- as(object, "DESeqDataSet")
            ## This step ensures we humanize the sample names, when possible.
            suppressMessages({
                dds <- convertSampleIDsToNames(dds)
            })
            res <- .joinRowData(object = res, DESeqDataSet = dds)
            res <- .joinCounts(object = res, DESeqDataSet = dds)
            assert(is(res, "DESeqResults"))
        }
        name <- contrastName(object, i = i)
        contrastName(res) <- name
        msg <- name
        if (isTRUE(lfcShrink)) {
            msg <- paste(msg, "(shrunken LFC)")
        }
        alert(msg)
        validObject(res)
        res
    }



#' @rdname results
#' @export
setMethod(
    f = "results",
    signature = signature("DESeqAnalysis"),
    definition = `results,DESeqAnalysis`
)
