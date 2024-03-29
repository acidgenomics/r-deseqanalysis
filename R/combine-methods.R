#' Combine multiple objects

#' @name combine
#' @note Updated 2022-03-08.
#'
#' @details
#' Combines the results from 2 separate `DESeqAnalysis` objects. Note that the
#' internal `DESeqDataSet` and `DESeqTransform` objects slotted in `x` and `y`
#' must be identical. The respective results must not contain any intersecting
#' names. The `lfcShrink` slot is optional but must be either defined in both
#' objects or `NULL` in both objects.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @return Modified object.
#'
#' @examples
#' data(deseq)
#'
#' ## DESeqAnalysis ====
#' x <- deseq
#' y <- deseq
#' resultsNames(x) <- paste0("x_", resultsNames(x))
#' resultsNames(y) <- paste0("y_", resultsNames(y))
#'
#' object <- combine(x, y)
#' resultsNames(object)
NULL



## Updated 2023-12-18.
`combine,DESeqAnalysis` <- # nolint
    function(x, y) {
        assert(
            validObject(x),
            validObject(y),
            areDisjointSets(resultsNames(x), resultsNames(y)),
            identical(x@data@assays, y@data@assays),
            identical(x@data@colData, y@data@colData),
            identical(x@data@design, y@data@design),
            identical(x@data@elementMetadata, y@data@elementMetadata),
            identical(x@data@metadata, y@data@metadata),
            identical(x@data@rowRanges, y@data@rowRanges),
            identical(x@transform, y@transform)
        )
        alertInfo("Combining results into single DESeqAnalysis object.")
        dl(c(
            "x" = printString(resultsNames(x)),
            "y" = printString(resultsNames(y))
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
        out <- DESeqAnalysis(
            data = data,
            transform = transform,
            results = results,
            lfcShrink = lfcShrink
        )
        if (!identical(names(metadata(x)), names(metadata(out)))) {
            diff <- setdiff(names(metadata(x)), names(metadata(out)))
            meta <- c(metadata(out), metadata(x)[diff])
            metadata(out) <- meta
        }
        out
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
