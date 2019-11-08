#' Correlation
#'
#' @name correlation
#' @inherit basejump::correlation
#' @note Updated 2019-11-08.
#'
#' @inheritParams acidroxygen::params
#' @param col `character(1)`.
#'   Column name.
#' @param ... Additional arguments.
#'
#' @examples
#' data(deseq, package = "acidtest")
#'
#' ## DESeqResults ====
#'
#'
#' ## DESeqAnalysis ====
#'
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
        message("Analyzing ", nrow(data), " genes.")
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
`correlation,DESeqAnalysis` <-  # nolint
    function(x, i, j, col = "log2FoldChange", method) {
        method <- match.arg(method)

        # correlation(
        #     x = results(x,
        # )

        print("hello world")
        ## correlation
    }

formals(`correlation,DESeqAnalysis`)[["method"]] <- method



rm(method)
