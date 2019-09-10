#' @name contrastName
#' @inherit bioverbs::contrastName
#' @note Updated 2019-09-10.
#'
#' @inheritParams acidroxygen::params
#' @inheritParams params
#' @param format `character(1)`.
#'   Name format to return:
#'
#'   - `resultsNames`: Attempt to matching the conventions in
#'     [`resultsNames()`][DESeq2::resultsNames].
#'   - `title`: Human readable, for plot titles and/or table captions.
#' @param ... Additional arguments.
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



#' @rdname contrastName
#' @name contrastName
#' @importFrom bioverbs contrastName
#' @usage contrastName(object, ...)
#' @export
NULL

#' @rdname contrastName
#' @name contrastName<-
#' @importFrom bioverbs contrastName<-
#' @usage contrastName(object, ...) <- value
#' @export
NULL



## Updated 2019-09-10.
`contrastName,DESeqResults` <-  # nolint
    function(object, format = c("resultsNames", "title")) {
        validObject(object)
        format <- match.arg(format)
        ## Use metadata stash, if defined. This is the recommended approach
        ## when passing off from DESeqAnalysis object, using `resultsNames()`.
        x <- metadata(object)[["contrastName"]]
        ## Otherwise, determine the contrast name automatically from mcols.
        if (is.null(x)) {
            ## See approach in `DESeq2::resultsNames()` on DESeqDataSet.
            x <- mcols(object, use.names = TRUE)
            x <- x["log2FoldChange", "description", drop = TRUE]
        }
        assert(isString(x))
        ## Always strip prefix, e.g. log2 fold change (MLE).
        x <- sub("^.*:\\s", "", x)
        if (identical(format, "resultsNames")) {
            x <- makeNames(x)
        } else if (identical(format, "title")) {
            ## Strip prefix, e.g. log2 fold change (MLE).
            x <- sub("^.*:\\s", "", x)
            ## Pad the first space with as a colon.
            x <- sub("\\s", " : ", x)
            ## Ensure "vs." contains a period.
            x <- sub("\\svs\\s", " vs. ", x)
            ## Improve appearance for difference of differences.
            x <- gsub("\\+", " \\+\n    ", x)
        }
        x
    }



#' @rdname contrastName
#' @export
setMethod(
    f = "contrastName",
    signature = signature("DESeqResults"),
    definition = `contrastName,DESeqResults`
)



## Updated 2019-09-10.
`contrastName<-,DESeqResults,character` <-  # nolint
    function(object, value) {
        assert(isString(value))
        metadata(object)[["contrastName"]] <- value
        validObject(object)
        object
    }



#' @rdname contrastName
#' @export
setReplaceMethod(
    f = "contrastName",
    signature = signature(
        object = "DESeqResults",
        value = "character"
    ),
    definition = `contrastName<-,DESeqResults,character`
)



## Updated 2019-07-23.
`contrastName,DESeqAnalysis` <-  # nolint
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
    definition = `contrastName,DESeqAnalysis`
)
