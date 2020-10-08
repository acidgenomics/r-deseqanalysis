#' Correlation
#'
#' @name correlation
#' @inherit basejump::correlation
#' @note Updated 2020-08-05.
#'
#' @inheritParams AcidRoxygen::params
#' @param col `character(1)`.
#'   Column name.
#' @param ... Additional arguments.
#'
#' @examples
#' data(deseq)
#'
#' ## DESeqAnalysis ====
#' correlation(deseq, i = 1L, j = 2L)
NULL



#' @rdname correlation
#' @name correlation
#' @importFrom basejump correlation
#' @usage correlation(x, y, ...)
#' @export
NULL



method <- formals(stats::cor)[["method"]]



## Updated 2019-11-08.
`correlation,DESeqResults,DESeqResults` <-  # nolint
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

formals(`correlation,DESeqResults,DESeqResults`)[["method"]] <- method



#' @rdname correlation
#' @export
setMethod(
    f = "correlation",
    signature = signature(
        x = "DESeqResults",
        y = "DESeqResults"
    ),
    definition = `correlation,DESeqResults,DESeqResults`
)



## Updated 2020-08-05.
`correlation,DESeqAnalysis,missing` <-  # nolint
    function(
        x,
        y = NULL,
        i,
        j,
        col = "log2FoldChange",
        method
    ) {
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
