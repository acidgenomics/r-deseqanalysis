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
        fill,
        flip = TRUE
    ) {
        validObject(object)
        assert(isFlag(flip))
        direction <- "both"
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
        data[["rowname"]] <- factor(
            x = data[["rowname"]],
            levels = levels
        )
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
                x = "contrast",
                y = "differentially expressed genes",
                fill = "direction"
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
