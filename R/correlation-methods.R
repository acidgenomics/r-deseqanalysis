#' Correlation
#'
#' @name correlation
#' @note Updated 2019-11-07.
#'
#' @inheritParams acidroxygen::params
#' @param i `integer(1)` or `character(1)`.
#'   Primary assay.
#' @param j `integer(1)`, `character(1)`, or `NULL`.
#'   Optional secondary assay.
#'   If `NULL`, calculates correlation matrix only on the primary assay.
#'
#' @seealso
#' - `stats::cor()`.
#' - `stats::cor.test()`.
#' - `stats::sd()`.
#' - `S4Vectors::cor()`.
#' - https://stats.stackexchange.com/questions/24980
#'
#' @examples
#' data(deseq)
#' dds <- as(deseq, "DESeqDataSet")
#' correlation(dds)
NULL



#' @importFrom S4Vectors cor
NULL



## Updated 2019-11-07.
`correlation,matrix` <-  # nolint
    function(
        x, y = NULL,
        method = c("pearson", "kendall", "spearman")
    ) {
        method <- match.arg(method)
        if (is.null(y)) {
            cor(x = x, y = y, method = method)
        } else {
            cor(x = c(x), y = c(y), method = method)
        }
    }



#' @rdname correlation
#' @export
setMethod(
    f = "correlation",
    signature = signature("matrix"),
    definition = `correlation,matrix`
)



## Updated 2019-11-07.
`correlation,Matrix` <- `correlation,matrix`



#' @rdname correlation
#' @export
setMethod(
    f = "correlation",
    signature = signature("Matrix"),
    definition = `correlation,Matrix`
)



## FIXME Allow the user to use x vs. y or i vs. j but not both.
## FIXME Don't allow the user to set the same i, j values.
## Updated 2019-11-07.
`correlation,SummarizedExperiment` <-  # nolint
    function(
        x, y = NULL,
        i = 1L, j = NULL,
        method = c("pearson", "kendall", "spearman")
    ) {
        method <- match.arg(method)
    }



## Updated 2019-11-07.
`correlation,DESeqResults` <-  # nolint
    function(x, y, col = "log2FoldChange") {
        print("hello world")
    }



## FIXME Don't allow user to set the same i, j values.
## Updated 2019-11-07.
`correlation,DESeqAnalysis` <-  # nolint
    function(x, i, j, col = "log2FoldChange") {
        print("hello world")
    }
