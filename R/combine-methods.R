#' @name combine
#' @inherit BiocGenerics::combine return title
#' @note Updated 2019-11-19.
#'
#' @details
#' Combines the results from 2 separate `DESeqAnalysis` objects. Note that the
#' internal `DESeqDataSet` and `DESeqTransform` objects slotted in `x` and `y`
#' must be identical. The respective results must not contain any intersecting
#' names. The `lfcShrink` slot is optional but must be either defined in both
#' objects or `NULL` in both objects.
#'
#' @inheritParams acidroxygen::params
#' @param ... Additional arguments.
#'
#' @examples
#' data(deseq)
#'
#' x <- deseq
#' y <- deseq
#' resultsNames(x) <- paste0("x_", resultsNames(x))
#' resultsNames(y) <- paste0("y_", resultsNames(y))
#'
#' object <- combine(x, y)
#' resultsNames(object)
NULL



#' @rdname combine
#' @name combine
#' @importFrom BiocGenerics combine
#' @usage combine(x, y, ...)
#' @export
NULL



## Updated 2020-01-03.
`combine,DESeqAnalysis` <-
    function(x, y) {
        validObject(x)
        validObject(y)
        assert(
            areDisjointSets(resultsNames(x), resultsNames(y)),
            identical(
                x = x@data@assays,
                y = y@data@assays
            ),
            identical(
                x = x@data@colData,
                y = y@data@colData
            ),
            identical(
                x = x@data@design,
                y = y@data@design
            ),
            identical(
                x = x@data@elementMetadata,
                y = y@data@elementMetadata
            ),
            identical(
                x = x@data@metadata,
                y = y@data@metadata
            ),
            identical(
                x = x@data@rowRanges,
                y = y@data@rowRanges
            ),
            identical(
                x = x@transform,
                y = y@transform
            )
        )
        message(paste(
            "Combining results into single DESeqAnalysis object.",
            "x:",
            printString(resultsNames(x)),
            "y:",
            printString(resultsNames(y)),
            sep = "\n"
        ))
        data <- x@data
        transform <- x@transform
        results <- c(x@results, y@results)
        if (is.null(x@lfcShrink)) {
            assert(is.null(y@lfcShrink))
            lfcShrink <- NULL
        } else {
            lfcShrink <- c(x@lfcShrink, y@lfcShrink)
        }
        DESeqAnalysis(
            data = data,
            transform = transform,
            results = results,
            lfcShrink = lfcShrink
        )
    }



#' @rdname combine
#' @export
setMethod(
    f = "combine",
    signature = signature(
        x = "DESeqAnalysis",
        y = "DESeqAnalysis"
    ),
    definition = `combine,DESeqAnalysis`
)
