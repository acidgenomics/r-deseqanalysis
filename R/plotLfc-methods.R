#' @name plotLfc
#' @inherit AcidGenerics::plotLfc
#' @note Updated 2022-05-17.
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
#' object <- deseq
#' lfcThreshold(object) <- 0.5
#' plotLfc(object)
NULL



## Updated 2023-08-11.
`plotLfc,DESeqAnalysis` <- # nolint
    function(object) {
        assert(validObject(object))
        lfcThreshold <- lfcThreshold(object)
        resList <- as.list(as(object, "DESeqResultsList"))
        data <- do.call(
            what = rbind,
            args = Map(
                contrast = names(resList),
                df = resList,
                f = function(contrast, df) {
                    data.frame(
                        "contrast" = contrast,
                        "log2FoldChange" = df[["log2FoldChange"]]
                    )
                }
            )
        )
        keep <- complete.cases(data)
        data <- data[keep, , drop = FALSE]
        keep <- abs(data[["log2FoldChange"]]) >= lfcThreshold
        data <- data[keep, , drop = FALSE]
        p <- ggplot(
            data = data,
            mapping = aes(
                x = .data[["log2FoldChange"]],
                y = after_stat(.data[["density"]])
            )
        ) +
            geom_freqpoly(
                stat = "bin",
                binwidth = 0.25,
                mapping = aes(
                    color = .data[["contrast"]]
                )
            ) +
            acid_scale_color_discrete()
        p
    }



#' @rdname plotLfc
#' @export
setMethod(
    f = "plotLfc",
    signature = signature(object = "DESeqAnalysis"),
    definition = `plotLfc,DESeqAnalysis`
)
