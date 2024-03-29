#' Stacked bar plot of DEGs
#'
#' @name plotDegStackedBar
#' @inherit AcidGenerics::plotDegStackedBar
#' @note Updated 2022-05-18.
#'
#' @inheritParams degPerContrast
#' @inheritParams AcidRoxygen::params
#' @inheritParams params
#' @param ... Additional arguments.
#'
#' @param label `logical(1)`.
#' Label the number of DEGs per contrast on the plot.
#'
#' @param orderBySize `logical(1)`.
#' Order contrasts by DEG set size.
#'
#' @examples
#' data(deseq)
#'
#' ## DESeqAnalysis ====
#' plotDegStackedBar(deseq)
NULL



## Updated 2023-12-18.
`plotDegStackedBar,DESeqAnalysis` <- # nolint
    function(object,
             i = NULL,
             direction = c("both", "up", "down"),
             orderBySize = FALSE,
             label = TRUE,
             flip = TRUE) {
        assert(
            validObject(object),
            isFlag(orderBySize),
            isFlag(label),
            isFlag(flip)
        )
        direction <- match.arg(direction)
        data <- degPerContrast(
            object = object,
            i = i,
            direction = direction,
            return = "matrix"
        )
        ## Reorder the factor levels, so we can rank from most DEG to least.
        if (isTRUE(orderBySize)) {
            levels <- names(sort(colSums(data), decreasing = TRUE))
        }
        data <- melt(
            object = data,
            colnames = c("direction", "contrast", "value")
        )
        if (isTRUE(orderBySize)) {
            data[["contrast"]] <-
                factor(x = data[["contrast"]], levels = levels)
        }
        p <- ggplot(
            data = as.data.frame(data),
            mapping = aes(
                x = .data[["contrast"]],
                y = .data[["value"]],
                fill = .data[["direction"]],
                label = .data[["value"]]
            )
        ) +
            geom_bar(
                color = "black",
                stat = "identity"
            )
        if (isTRUE(label)) {
            p <- p + geom_text(
                size = 3L,
                position = position_stack(vjust = 0.5)
            )
        }
        p <- p + labs(
            x = "contrast",
            y = "differentially expressed genes",
            fill = "direction",
            title = "Differentially expressed genes per contrast",
            subtitle = .thresholdLabel(
                object = object,
                direction = direction,
                alphaThreshold = alphaThreshold(object),
                baseMeanThreshold = baseMeanThreshold(object),
                lfcShrinkType = lfcShrinkType(object),
                lfcThreshold = lfcThreshold(object)
            )
        )
        ## Color palette.
        p <- p + acid_scale_fill_discrete()
        if (isTRUE(flip)) {
            p <- p + acid_discrete_coord_flip()
        }
        p
    }



#' @rdname plotDegStackedBar
#' @export
setMethod(
    f = "plotDegStackedBar",
    signature = signature(object = "DESeqAnalysis"),
    definition = `plotDegStackedBar,DESeqAnalysis`
)
