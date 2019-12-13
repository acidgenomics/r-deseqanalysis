#' Stacked bar plot of DEGs
#'
#' @name plotDEGStackedBar
#' @note Updated 2019-12-13.
#'
#' @inheritParams acidroxygen::params
NULL



## Updated 2019-12-13.
`plotDEGStackedBar,DESeqAnalysis` <-  # nolint
    function(
        object,
        alpha = NULL,
        lfcThreshold = NULL,
        color
    ) {
        validObject(object)
        direction <- match.arg(direction)
        degPerContrast <- .degPerContrast(
            object = object,
            alpha = alpha,
            lfcThreshold = lfcThreshold,
            direction = direction,
            n = TRUE
        )
        data <- as.data.frame(degPerContrast)
        ## Reorder the factor levels, so we can rank from most DEG to least.
        levels <- names(sort(colSums(data), decreasing = TRUE))
        data <- as.data.frame(melt(
            object = t(data),
            colnames = c("rowname", "colname", "value")
        ))
        assert(is.factor(data[["rowname"]]))
        data[["rowname"]] <- factor(data[["rowname"]], levels = levels)
        p <- ggplot(
            data = data,
            mapping = aes(
                x = !!sym("rowname"),
                y = !!sym("value"),
                fill = !!sym("colname"),
                label = !!sym("value")
            )
        ) +
            geom_bar(stat = "identity") +
            geom_text(
                size = 3,
                position = position_stack(vjust = 0.5)
            ) +
            labs(
                x = "sample",
                y = "n",
                fill = "direction"
            )
        p <- acidplots::acid_coord_flip(p)
        ## Color.
        if (is(color, "ScaleDiscrete")) {
            p <- p + color
        }
        p
    }

formals(`plotDEGStackedBar,DESeqAnalysis`)[["color"]] <-
    formalsList[["color.discrete"]]



#' @rdname plotDEGStackedBar
#' @export
setMethod(
    f = "plotDEGStackedBar",
    signature = signature("DESeqAnalysis"),
    definition = `plotDEGStackedBar,DESeqAnalysis`
)
