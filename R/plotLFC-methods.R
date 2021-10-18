## FIXME Need to add support for this.



#' @name plotLFC
#' @inherit AcidGenerics::plotLFC
#' @note Updated 2021-10-18.
#'
#' @inheritParams AcidRoxygen::params
#' @inheritParams params
#' @param ... Additional arguments.
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
    function(
        object
    ) {
        abort("FIXME Need to add method support.")
}



## Updated 2021-08-03.
`plotLFC,DESeqAnalysis` <-  # nolint
    function(
        object,
        i  # FIXME
    ) {
        abort("FIXME Need to add method support.")
    }



#' @rdname plotLFC
#' @export
setMethod(
    f = "plotLFC",
    signature = signature(object = "DESeqAnalysis"),
    definition = `plotLFC,DESeqAnalysis`
)

#' @rdname plotLFC
#' @export
setMethod(
    f = "plotLFC",
    signature = signature(object = "DESeqResults"),
    definition = `plotLFC,DESeqResults`
)
