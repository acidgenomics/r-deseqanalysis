## FIXME Need to add support for this.



#' Plot log fold change
#'
#' @name plotLFC
#' @note Updated 2021-08-03.
#'
#' @inheritParams params
#'
#' @examples
#' data(deseq)
#'
#' ## Plot the LFC distribution.
#' plotLFC(deseq, i = 1L)
#'
#' ## Plot expression of specific genes.
#' dds <- as(deseq, "DESeqDataSet")
#' genes <- head(rownames(dds))
#' plotLFC(deseq, i = 1L, genes = genes)
NULL



## Updated 2021-08-03.
`plotLFC,DESeqResults` <-  # nolint
    function(object) {
        stop("FIXME Need to add method support.")
}



## Updated 2021-08-03.
`plotLFC,DESeqAnalysis` <-  # nolint
    function(object) {
        stop("FIXME Need to add method support.")
    }
