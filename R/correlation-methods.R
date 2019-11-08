#' Correlation
#'
#' @name correlation
#' @inherit basejump::correlation
#' @note Updated 2019-11-08.
#'
#' @inheritParams acidroxygen::params
#'
#' @examples
#' data(correlation, package = "acidtest")
#' list <- correlation
#' rm(correlation)
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
#' @importFrom bioverbs correlation
#' @usage correlation(x, y, ...)
#' @export
NULL



## Updated 2019-11-08.
`correlation,DESeqResults` <-  # nolint
    function(x, y, col = "log2FoldChange") {
        print("hello world")
        ## correlation
    }



## Updated 2019-11-08.
`correlation,DESeqAnalysis` <-  # nolint
    function(x, i, j, col = "log2FoldChange") {
        print("hello world")
        ## correlation
    }
