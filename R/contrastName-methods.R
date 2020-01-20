#' @name contrastName
#' @inherit acidgenerics::contrastName
#' @note Updated 2019-11-08.
#'
#' @inheritParams acidroxygen::params
#' @inheritParams params
#' @param format `character(1)`.
#'   Name format to return:
#'
#'   - `resultsNames`: Attempt to matching the conventions in
#'     [`resultsNames()`][DESeq2::resultsNames].
#'   - `title`: Human readable, for plot titles and/or table captions.
#' @param useStash `logical(1)`.
#'   Check for `contrastName` metadata stash in `DESeqResults` object. Intended
#'   for use with `DESeqAnalysis` methods.
#' @param ... Additional arguments.
#'
#' @seealso
#' - [`contrastNames()`].
#' - [`resultsNames()`][DESeq2::resultsNames].
#'
#' @examples
#' data(deseq)
#'
#' ## DESeqResults ====
#' object <- as(deseq, "DESeqResults")
#' contrastName(object)
#'
#' ## DESeqAnalysis ====
#' contrastName(deseq, i = 1L)
NULL



#' @rdname contrastName
#' @name contrastName
#' @importFrom acidgenerics contrastName
#' @usage contrastName(object, ...)
#' @export
NULL

#' @rdname contrastName
#' @name contrastName<-
#' @importFrom acidgenerics contrastName<-
#' @usage contrastName(object, ...) <- value
#' @export
NULL



## Updated 2019-09-11.
`contrastName,DESeqResults` <-  # nolint
    function(
        object,
        format = c("resultsNames", "title"),
        useStash = TRUE
    ) {
        validObject(object)
        assert(isFlag(useStash))
        format <- match.arg(format)
        ## Use metadata stash, if defined. This is the recommended approach
        ## when passing off from DESeqAnalysis object, using `resultsNames()`.
        if (isTRUE(useStash)) {
            x <- metadata(object)[["contrastName"]]
        } else {
            x <- NULL
        }
        ## Otherwise, determine the contrast name automatically from mcols.
        ## See approach in `DESeq2::resultsNames()` on DESeqDataSet.
        if (is.null(x)) {
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



## This method is to be used primarily to set the contrast name on DESeqResults
## inside plotting functions. See `plotMA()` method, for example.
## Updated 2019-11-08.
`contrastName,DESeqAnalysis` <-  # nolint
    function(object, i, ...) {
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
        contrastNames <- contrastNames(object)
        if (isString(i)) {
            x <- i
            assert(isSubset(x, contrastNames))
        } else {
            x <- contrastNames[[i]]
        }
        assert(isString(x))
        x
    }



#' @rdname contrastName
#' @export
setMethod(
    f = "contrastName",
    signature = signature("DESeqAnalysis"),
    definition = `contrastName,DESeqAnalysis`
)
