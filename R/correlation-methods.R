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
#' - `S4Vectors::cor()`.
#'
#' @examples
#' data(deseq)
#' dds <- as(deseq, "DESeqDataSet")
#' correlation(dds)
NULL



#' @importFrom S4Vectors cor
NULL



## FIXME Allow the user to use x vs. y or i vs. j but not both.

`correlation,SummarizedExperiment` <-  # nolint
    function(
        x,
        y = NULL,
        i = 1L,
        j = NULL,
        method = c("pearson", "kendall", "spearman")
    ) {
        method <- match.arg(method)

    }
