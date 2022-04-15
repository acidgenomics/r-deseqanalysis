#' @name plotLFC
#' @inherit AcidGenerics::plotLFC
#' @note Updated 2022-04-15.
#'
#' Plot the log2 fold change distributions for all contrasts in the analysis.
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
NULL



## Updated 2022-04-15.
`plotLFC,DESeqAnalysis` <- # nolint
    function(object) {
        validObject(object)
        resList <- as.list(as(object, "DESeqResultsList"))
        data <- do.call(
            what = rbind,
            args = mapply(
                contrast = names(resList),
                df = resList,
                FUN = function(contrast, df) {
                    data.frame(
                        "contrast" = contrast,
                        "log2FoldChange" = df[["log2FoldChange"]]
                    )
                },
                SIMPLIFY = FALSE,
                USE.NAMES = FALSE
            )
        )
        p <- ggplot(
            data = data,
            mapping = aes(x = !!sym("log2FoldChange"))
        ) +
            geom_density(
                mapping = aes(
                    color = !!sym("contrast")
                ),
                fill = NA
            ) +
            autoDiscreteColorScale()
        p
    }



#' @rdname plotLFC
#' @export
setMethod(
    f = "plotLFC",
    signature = signature(object = "DESeqAnalysis"),
    definition = `plotLFC,DESeqAnalysis`
)
