## FIXME Need to add support for this.
## FIXME Consider plotting all fold changes on a single graph...



#' @name plotLFC
#' @inherit AcidGenerics::plotLFC
#' @note Updated 2022-03-30.
#'
#' Plot the log2 fold change distribution per contrast.
#'
#' @inheritParams AcidRoxygen::params
#' @inheritParams params
#' @param ... Additional arguments.
#'
#' @examples
#' data(deseq)
#'
#' ## DESeqAnalysis ====
#' plotLFC(deseq)
#'
#' ## Plot expression of specific genes.
#' genes <- head(rownames(as.DESeqDataSet(deseq)))
#' plotLFC(deseq, genes = genes)
NULL



## Updated 2022-03-30.
`plotLFC,DESeqAnalysis` <-  # nolint
    function(object) {
        validObject(object)
        resList <- as.list(as(object, "DESeqResultsList"))

        tbl <- mapply(
            contrast = names(resList),
            df = resList,
            FUN = function(contrast, df) {
                tibble(
                    "contrast" = contrast,
                    "log2FoldChange" = df[["log2FoldChange"]]
                )
            },
            SIMPLIFY = FALSE
        )


        df <- lapply(
            X =
        )
        ## Unlist and create a data.frame of values.


        abort("FIXME Need to add method support.")
    }



#' @rdname plotLFC
#' @export
setMethod(
    f = "plotLFC",
    signature = signature(object = "DESeqAnalysis"),
    definition = `plotLFC,DESeqAnalysis`
)
