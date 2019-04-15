#' @name results
#' @inherit bioverbs::results
#' @inheritParams basejump::params
#' @inheritParams params
NULL



#' @rdname results
#' @name results
#' @importFrom bioverbs results
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
    function(object, results, lfcShrink = TRUE) {
        .matchResults(
            object = object,
            results = results,
            lfcShrink = lfcShrink
        )
    }



#' @rdname results
#' @export
setMethod(
    f = "results",
    signature = signature("DESeqAnalysis"),
    definition = results.DESeqAnalysis
)
