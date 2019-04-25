#' @name contrastName
#' @inherit bioverbs::contrastName
#' @inheritParams basejump::params
#' @inheritParams params
#'
#' @param format `character(1)`.
#'   Name format to return:
#'
#'   - `resultsNames`: Attempt to matching the conventions in
#'     [`resultsNames()`][DESeq2::resultsNames].
#'   - `title`: Human readable, for plot titles and/or table captions.
#'
#' @seealso [`resultsNames()`][DESeq2::resultsNames].
#'
#' @examples
#' data(deseq)
#'
#' ## DESeqResults ====
#' object <- as(deseq, "DESeqResults")
#' contrastName(object)
#'
#' ## DESeqAnalysis ====
#' contrastName(deseq, results = 1L)
NULL



contrastName.DESeqResults <-  # nolint
    function(object, format = c("resultsNames", "title")) {
        validObject(object)
        format <- match.arg(format)
        x <- mcols(object)["log2FoldChange", "description", drop = TRUE]
        assert(isCharacter(x))
        # Always strip prefix, e.g. log2 fold change (MLE).
        x <- sub("^.*:\\s", "", x)
        if (format == "resultsNames") {
            makeNames(x)
        } else if (format == "title") {
            x %>%
                # Strip prefix, e.g. log2 fold change (MLE).
                sub("^.*:\\s", "", .) %>%
                # Pad the first space with as a colon.
                sub("\\s", " : ", .) %>%
                sub("\\svs\\s", " vs. ", .) %>%
                # Improve appearance for difference of differences.
                gsub("\\+", " \\+\n    ", .)
        }
    }



#' @rdname contrastName
#' @export
setMethod(
    f = "contrastName",
    signature = signature("DESeqResults"),
    definition = contrastName.DESeqResults
)



contrastName.DESeqAnalysis <-  # nolint
    function(object, results) {
        suppressMessages(
            results <- results(object = object, results = results)
        )
        do.call(
            what = contrastName,
            args = list(object = results)
        )
    }



#' @rdname contrastName
#' @export
setMethod(
    f = "contrastName",
    signature = signature("DESeqAnalysis"),
    definition = contrastName.DESeqAnalysis
)
