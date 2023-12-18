## FIXME This doesn't work for old DESeqDataSet objects, need to rethink.
## Error in h(simpleError(msg, call)) :
##   error in evaluating the argument 'object' in selecting a method for function 'makeNames': invalid class “DESeqResults” object: slots in class definition but not in object: "priorInfo"
## Calls: DESeqAnalysis ... contrastName -> contrastName -> .local -> validObject



#' @name contrastName
#' @inherit AcidGenerics::contrastName
#' @note Updated 2021-03-15.
#'
#' @inheritParams AcidRoxygen::params
#' @inheritParams params
#' @param ... Additional arguments.
#'
#' @param format `character(1)`.
#' Name format to return:
#' - `resultsNames`: Attempt to matching the conventions in
#' [`resultsNames()`][DESeq2::resultsNames].
#' - `title`: Human readable, for plot titles and/or table captions.
#'
#' @seealso
#' - [`resultsNames()`].
#'
#' @examples
#' data(deseq)
#'
#' ## DESeqAnalysis ====
#' contrastName(deseq, i = 1L)
NULL



## This method is to be used primarily to set the contrast name on DESeqResults
## inside plotting functions. See `plotMa()` method, for example.
## Updated 2021-03-15.
`contrastName,DESeqAnalysis` <- # nolint
    function(object, i, ...) {
        assert(isScalar(i))
        resList <- DESeqResultsList(object, quiet = TRUE)
        res <- resList[[i]]
        assert(is(res, "DESeqResults"))
        contrastName(res, ...)
    }



## Updated 2021-03-15.
`contrastName,DESeqResults` <- # nolint
    function(object,
             format = c("resultsNames", "title")) {
        validObject(object)
        format <- match.arg(format)
        ## Use metadata stash, if defined. This is the recommended approach
        ## when passing off from DESeqAnalysis object, using `resultsNames()`.
        x <- metadata(object)[["contrastName"]]
        ## Otherwise, determine the contrast name automatically from mcols.
        ## See approach in `DESeq2::resultsNames()` on DESeqDataSet.
        if (is.null(x)) {
            x <- mcols(object, use.names = TRUE)
            x <- x["log2FoldChange", "description", drop = TRUE]
        }
        assert(isString(x))
        ## Always strip prefix, e.g. log2 fold change (MLE).
        x <- sub("^.*:\\s", "", x)
        switch(
            EXPR = format,
            "resultsNames" = {
                x <- makeNames(x)
            },
            "title" = {
                ## Strip prefix, e.g. log2 fold change (MLE).
                x <- sub("^.*:\\s", "", x)
                ## Pad the first space with as a colon.
                x <- sub("\\s", " : ", x)
                ## Ensure "vs." contains a period.
                x <- sub("\\svs\\s", " vs. ", x)
                ## Improve appearance for difference of differences.
                x <- gsub("\\+", " \\+\n    ", x)
            }
        )
        x
    }



## Updated 2019-09-10.
`contrastName<-,DESeqResults,character` <- # nolint
    function(object, value) {
        assert(isString(value))
        metadata(object)[["contrastName"]] <- value
        validObject(object)
        object
    }



#' @rdname contrastName
#' @export
setMethod(
    f = "contrastName",
    signature = signature(object = "DESeqAnalysis"),
    definition = `contrastName,DESeqAnalysis`
)

#' @rdname contrastName
#' @export
setMethod(
    f = "contrastName",
    signature = signature(object = "DESeqResults"),
    definition = `contrastName,DESeqResults`
)



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
