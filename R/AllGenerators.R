#' @include AllGenerics.R
NULL



## DESeqAnalysis ===============================================================
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
#' rowRanges <- basejump::emptyRanges(names = rownames(data))
#' mcols(rowRanges)[["geneId"]] <- paste0("id", seq_len(length(rowRanges)))
#' mcols(rowRanges)[["geneName"]] <- paste0("name", seq_len(length(rowRanges)))
#' rowRanges(data) <- rowRanges
#' data <- DESeq2::DESeq(data)
#' class(data)
#'
#' transform <- DESeq2::varianceStabilizingTransformation(data)
#' class(transform)
#'
#' resultsNames(data)
#' name <- resultsNames(data)[[2L]]
#' results <- DESeq2::results(data, name = name)
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
#' object <- DESeqAnalysis(
#'     data = data,
#'     transform = transform,
#'     results = results,
#'     lfcShrink = lfcShrink
#' )
#' print(object)
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



## DESeqAnalysisList ===========================================================
#' @name DESeqAnalysisList
#' @inherit DESeqAnalysisList-class title description return
#' @note Updated 2021-03-08.
#'
#' @param ... `DESeqAnalysis` objects or named `list`.
#'
#' @examples
#' data(deseq)
#' x <- DESeqAnalysisList(deseq)
#' x
NULL



## Updated 2021-03-08.
`DESeqAnalysisList,list` <- function(object) {  # nolint
    assert(hasLength(object), hasNames(object))
    new(Class = "DESeqAnalysisList", object)
}



## Updated 2021-03-08.
`DESeqAnalysisList,SimpleList` <-  # nolint
    `DESeqAnalysisList,list`



## How to get names of dot arguments.
## https://stackoverflow.com/questions/51259346

## Updated 2021-03-08.
`DESeqAnalysisList,DESeqAnalysis` <-  # nolint
    function(object, ...) {
        mc <- match.call(expand.dots = FALSE)
        l <- append(x = list(object), values = list(...))
        names(l) <- c(
            as.character(mc[[2L]]),
            as.character(mc[["..."]])
        )
        new(Class = "DESeqAnalysisList", l)
    }



## Updated 2021-03-08.
`DESeqAnalysisList,missing` <-  # nolint
    function(object) {
        new(Class = "DESeqAnalysisList", list())
    }



#' @rdname DESeqAnalysisList
#' @export
setMethod(
    f = "DESeqAnalysisList",
    signature = signature("list"),
    definition = `DESeqAnalysisList,list`
)



#' @rdname DESeqAnalysisList
#' @export
setMethod(
    f = "DESeqAnalysisList",
    signature = signature("SimpleList"),
    definition = `DESeqAnalysisList,SimpleList`
)



#' @rdname DESeqAnalysisList
#' @export
setMethod(
    f = "DESeqAnalysisList",
    signature = signature("DESeqAnalysis"),
    definition = `DESeqAnalysisList,DESeqAnalysis`
)



#' @rdname DESeqAnalysisList
#' @export
setMethod(
    f = "DESeqAnalysisList",
    signature = signature("missing"),
    definition = `DESeqAnalysisList,missing`
)



## DESeqResultsList ============================================================
#' @name DESeqResultsList
#' @inherit DESeqResultsList-class title description return
#' @note Updated 2021-03-08.
#'
#' @param ... `DESeqResults` objects or named `list`.
#'
#' @examples
#' data(deseq)
#' x <- DESeqResultsList(deseq)
#' x


## Updated 2021-03-08.
`DESeqResultsList,missing` <-  # nolint
    function(object) {
        new(Class = "DESeqAnalysisList", list())
    }



## FIXME ALLOW THIS TO DISPATCH ON DESEQANALYSIS?


`DESeqResultsList,list` <- function(...) {  # nolint
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
