#' Correlation
#'
#' @name correlation
#' @inherit basejump::correlation
#' @note Updated 2019-11-12.
#'
#' @inheritParams acidroxygen::params
#' @param col `character(1)`.
#'   Column name.
#' @param ... Additional arguments.
#'
#' @examples
#' ## Working example currently only has 1 contrast slotted.
#' data(deseq)
#'
#' ## DESeqResults ====
#' ## > x <- results(deseq, i = 1L)
#' ## > y <- results(deseq, i = 2L)
#' ## > correlation(x = x, y = y)
#'
#' ## DESeqAnalysis ====
#' ## > x <- deseq
#' ## > correlation(x = x, i = 1L, j = 2L)
NULL



#' @rdname correlation
#' @name correlation
#' @importFrom basejump correlation
#' @usage correlation(x, y, ...)
#' @export
NULL



method <- formals(stats::cor)[["method"]]



## Updated 2019-11-08.
`correlation,DESeqResults` <-  # nolint
    function(x, y, col = "log2FoldChange", method) {
        assert(
            hasRownames(x),
            hasRownames(y),
            areIntersectingSets(x = rownames(x), y = rownames(y)),
            isString(col),
            isSubset(col, colnames(x)),
            isSubset(col, colnames(y))
        )
        method <- match.arg(method)
        ## Ensure that we're only comparing genes in common.
        keep <- intersect(x = rownames(x), y = rownames(y))
        data <- DataFrame(
            x = x[keep, col, drop = TRUE],
            y = y[keep, col, drop = TRUE],
            row.names = rownames(x)
        )
        ## Ensure that no NA values propagate.
        data <- data[complete.cases(data), , drop = FALSE]
        correlation(x = data[["x"]], y = data[["y"]], method = method)
    }

formals(`correlation,DESeqResults`)[["method"]] <- method



#' @rdname correlation
#' @export
setMethod(
    f = "correlation",
    signature = signature(
        x = "DESeqResults",
        y = "DESeqResults"
    ),
    definition = `correlation,DESeqResults`
)



## Updated 2019-11-08.
`correlation,DESeqAnalysis,missing` <-  # nolint
    function(x, y = NULL, i, j, col = "log2FoldChange", method) {
        assert(!identical(i, j))
        method <- match.arg(method)
        correlation(
            x = results(object = x, i = i),
            y = results(object = x, i = j),
            method = method
        )
    }

formals(`correlation,DESeqAnalysis,missing`)[["method"]] <- method



#' @rdname correlation
#' @export
setMethod(
    f = "correlation",
    signature = signature(
        x = "DESeqAnalysis",
        y = "missingOrNULL"
    ),
    definition = `correlation,DESeqAnalysis,missing`
)



rm(method)
