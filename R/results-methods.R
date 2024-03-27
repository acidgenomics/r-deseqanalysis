#' Results
#'
#' @name results
#' @note Updated 2023-12-18.
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
#' @param ... Additional arguments.
#'
#' @param extra `logical(1)`.
#' Include row data (i.e. gene metadata) and normalized counts from the
#' internal `DESeqDataSet`.
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



## Updated 2023-12-18.
`results,DESeqAnalysis` <- # nolint
    function(object,
             i,
             lfcShrink = NULL,
             extra = FALSE,
             quiet = TRUE) {
        assert(
            validObject(object),
            isScalar(i),
            isFlag(extra),
            isFlag(quiet)
        )
        if (isCharacter(i)) {
            assert(isSubset(i, resultsNames(object)))
        }
        resList <- DESeqResultsList(
            object = object,
            lfcShrink = lfcShrink,
            quiet = TRUE
        )
        lfcShrink <- metadata(resList)[["lfcShrink"]]
        res <- resList[[i]]
        assert(
            is(resList, "DESeqResultsList"),
            isFlag(lfcShrink),
            is(res, "DESeqResults")
        )
        if (isTRUE(extra)) {
            dds <- as(object, "DESeqDataSet")
            ## This step ensures we humanize the sample names, when possible.
            suppressMessages({
                dds <- convertSampleIdsToNames(dds)
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
        if (isFALSE(quiet)) {
            alert(msg)
        }
        res
    }



## Updated 2019-07-23.
`results,DESeqDataSet` <- # nolint
    function(object, ...) {
        DESeq2::results(object, ...)
    }



#' @rdname results
#' @export
setMethod(
    f = "results",
    signature = signature(object = "DESeqAnalysis"),
    definition = `results,DESeqAnalysis`
)

#' @describeIn results Arguments pass through to `DESeq2::results()`.
#' @export
setMethod(
    f = "results",
    signature = signature(object = "DESeqDataSet"),
    definition = `results,DESeqDataSet`
)
