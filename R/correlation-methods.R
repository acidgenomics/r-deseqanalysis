#' Correlation
#'
#' @name correlation
#' @inherit basejump::correlation
#' @note Updated 2019-11-08.
#'
#' @inheritParams acidroxygen::params
#' @param ... Additional arguments.
#'
#' @examples
#' data(correlation, package = "acidtest")
#' list <- correlation
#'
#' ## DESeqResults ====
#' x <- list[["vector_x"]]
#' y <- list[["vector_y"]]
#'
#' head(x)
#' head(y)
#'
#' correlation(x = x, y = y)
#'
#' ## DESeqAnalysis ====
#' x <- list[["matrix_x"]]
#' y <- list[["matrix_y"]]
#'
#' head(x)
#' head(y)
#'
#' stats::cor(x)
#' correlation(x)
#'
#' stats::cor(x = c(x), y = c(y))
#' correlation(x = x, y = y)
#'
#' ## SummarizedExperiment ====
#' x <- list[["SummarizedExperiment_x"]]
#' y <- list[["SummarizedExperiment_y"]]
#'
#' correlation(x = x, i = 1L)
#' correlation(x = x, i = 1L, j = 2L)
#' correlation(x = x, y = y)
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
        method <- match.arg(method)
        correlation(
            x = x[[col]],
            y = y[[col]],
            method = method
        )
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
