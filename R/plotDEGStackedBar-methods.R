#' Stacked bar plot of DEGs
#'
#' @name plotDEGStackedBar
#' @inherit AcidGenerics::plotDEGStackedBar
#' @note Updated 2020-09-21.
#'
#' @inheritParams degPerContrast
#' @inheritParams AcidRoxygen::params
#' @inheritParams params
#' @param orderBySize `logical(1)`.
#'   Order contrasts by DEG set size.
#' @param label `logical(1)`.
#'   Label the number of DEGs per contrast on the plot.
#' @param ... Additional arguments.
#'
#' @examples
#' data(deseq)
#'
#' ## DESeqAnalysis ====
#' plotDEGStackedBar(deseq)
NULL



## Updated 2020-09-21.
`plotDEGStackedBar,DESeqAnalysis` <-  # nolint
    function(
        object,
        i = NULL,
        direction = c("both", "up", "down"),
        orderBySize = FALSE,
        label = TRUE,
        fill,
        flip = TRUE
    ) {
        validObject(object)
        assert(
            isFlag(orderBySize),
            isFlag(label),
            isFlag(flip)
        )
        direction <- match.arg(direction)
        mat <- degPerContrast(
            object = object,
            i = i,
            direction = direction,
            return = "matrix"
        )
        data <- as.data.frame(mat)
        ## Reorder the factor levels, so we can rank from most DEG to least.
        if (isTRUE(orderBySize)) {
            levels <- names(sort(colSums(data), decreasing = TRUE))
        }
        data <- as.data.frame(melt(
            object = t(data),
            colnames = c("rowname", "colname", "value")
        ))
        assert(is.factor(data[["rowname"]]))
        if (isTRUE(orderBySize)) {
            data[["rowname"]] <- factor(x = data[["rowname"]], levels = levels)
        }
        p <- ggplot(
            data = data,
            mapping = aes(
                x = !!sym("rowname"),
                y = !!sym("value"),
                fill = !!sym("colname"),
                label = !!sym("value")
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
            ## FIXME REWORK THIS.
            subtitle = .thresholdLabel(
                n = NULL,
                direction = direction,
                alphaThreshold = alphaThreshold(object),
                lfcShrinkType = lfcShrinkType(object),
                lfcThreshold = lfcThreshold(object),
                baseMeanThreshold = baseMeanThreshold(object)
            )
        )
        if (isTRUE(flip)) {
            p <- acid_coord_flip(p)
        }
        ## Fill.
        if (is(fill, "ScaleDiscrete")) {
            p <- p + fill
        }
        p
    }

formals(`plotDEGStackedBar,DESeqAnalysis`)[["fill"]] <-
    formalsList[["fill.discrete"]]



#' @rdname plotDEGStackedBar
#' @export
setMethod(
    f = "plotDEGStackedBar",
    signature = signature("DESeqAnalysis"),
    definition = `plotDEGStackedBar,DESeqAnalysis`
)
