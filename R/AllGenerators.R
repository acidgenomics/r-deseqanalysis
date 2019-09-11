#' @inherit DESeqAnalysis-class title description return
#' @export
#' @note Updated 2019-08-20.
#'
#' @param data `DESeqDataSet`.
#' @param transform `DESeqTransform`.
#'   [DESeq2::varianceStabilizingTransformation()] recommended by default.
#' @param results `list` or single `DESeqResults`.
#'   One or more unshrunken `DESeqResults`.
#'   Assign the [DESeq2::results()] return here.
#' @param lfcShrink `list`, single `DESeqResults`, or `NULL`.
#'   *Optional*. One or more shrunken `DESeqResults`.
#'   Assign the [DESeq2::lfcShrink()] return here.
#'
#' @examples
#' data <- DESeq2::makeExampleDESeqDataSet()
#' data <- DESeq2::DESeq(data)
#' class(data)
#'
#' transform <- DESeq2::varianceStabilizingTransformation(data)
#' class(transform)
#'
#' resultsNames(data)
#' name <- resultsNames(data)[[2L]]
#' results <- results(data, name = name)
#' class(results)
#'
#' lfcShrink <- DESeq2::lfcShrink(dds = data, res = results, coef = 2L)
#'
#' results <- list(results)
#' names(results) <- name
#'
#' lfcShrink <- list(lfcShrink)
#' names(lfcShrink) <- name
#'
#' identical(names(results), names(lfcShrink))
#'
#' x <- DESeqAnalysis(
#'     data = data,
#'     transform = transform,
#'     results = results,
#'     lfcShrink = lfcShrink
#' )
#' print(x)
DESeqAnalysis <-  # nolint
    function(
        data,
        transform,
        results,
        lfcShrink = NULL
    ) {
        metadata <- list(version = .version)
        ## Allow input of single `DESeqResults`.
        if (is(results, "DESeqResults")) {
            results <- .coerceResultsToList(results)
        }
        if (is(lfcShrink, "DESeqResults")) {
            lfcShrink <- .coerceResultsToList(lfcShrink)
        }
        ## Automatically convert `lfcShrink = NULL` to empty list.
        if (is.null(lfcShrink)) {
            lfcShrink <- list()
        }
        new(
            Class = "DESeqAnalysis",
            data = data,
            transform = transform,
            results = results,
            lfcShrink = lfcShrink,
            metadata = metadata
        )
    }



## Note that this will automatically assign name.
## Updated 2019-07-23.
.coerceResultsToList <- function(from) {
    assert(is(from, "DESeqResults"))
    to <- list(from)
    names(to) <- makeNames(contrastName(from))
    to
}



## How to get names of dot arguments.
## https://stackoverflow.com/questions/51259346



#' @inherit DESeqAnalysisList-class title description return
#' @export
#' @note Updated 2019-08-20.
#' @param ... `DESeqAnalysis` objects.
#' @examples
#' data(deseq)
#' x <- DESeqAnalysisList(deseq)
#' x
DESeqAnalysisList <- function(...) {  # nolint
    mc <- match.call(expand.dots = FALSE)
    dots <- list(...)
    dotsNames <- as.character(mc[["..."]])
    ## Look to see if the user passed in a list.
    if (
        hasLength(dots, n = 1L) &&
        is.list(dots[[1L]])
    ) {
        data <- dots[[1L]]
    } else {
        data <- dots
        ## Here we're capturing the object names if the user doesn't pass the
        ## arguments in as named key value pairs.
        if (is.null(names(data))) {
            names(data) <- dotsNames
        }
    }
    new(Class = "DESeqAnalysisList", data)
}
