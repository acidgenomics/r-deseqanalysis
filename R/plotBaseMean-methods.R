## Note that `NULL` arguments aren't passing through as expected with
## `matchArgsToDoCall()`. Look into fixing this in a future goalie release.
## For example, `color = NULL` doesn't pass as expected.



#' Plot base mean distribution
#'
#' The base mean is the mean of normalized counts of all samples, normalizing
#' for sequencing depth.
#'
#' @name plotBaseMean
#' @note Updated 2019-10-15.
#'
#' @inheritParams acidroxygen::params
#' @inheritParams params
#' @param nonzero `logical(1)`.
#'   Remove zero-count features (genes).
#' @param summary `logical(1)`.
#'   Include distribution summary statistics as lines on the plot.
#'
#' @seealso
#' - https://support.bioconductor.org/p/75244/
#' - [`summary()`][base::summary]
#'
#' @return `ggplot`.
#'
#' @examples
#' data(deseq)
#' plotBaseMean(deseq)
NULL



## If outliers have been replaced:
## nolint start
## > all(rowMeans(counts(dds, normalized=TRUE, replaced=TRUE)) == res$baseMean)
## nolint end



## Updated 2019-10-15.
`plotBaseMean,numeric` <-  # nolint
    function(
        object,
        nonzero = TRUE,
        trans = c("log10", "log2", "identity"),
        summary = TRUE,
        color,
        labels = list(
            title = "Base mean distribution",
            subtitle = NULL,
            x = "average expression across all samples",
            y = "density",
            color = "summary"
        )
    ) {
        assert(
            is.numeric(object),
            isFlag(nonzero),
            isFlag(summary),
            isGGScale(color, scale = "discrete", aes = "color", nullOK = TRUE)
        )
        trans <- match.arg(trans)
        labels <- matchLabels(
            labels = labels,
            choices = eval(formals()[["labels"]])
        )
        ## Drop zero values prior to plot.
        if (isTRUE(nonzero)) {
            keep <- object > 0L
            ## Inform the user about how many zero count features were dropped.
            if (any(!keep)) {
                n <- sum(!keep, na.rm = TRUE)
                message(sprintf(
                    "Removing %d zero-count %s.",
                    n,
                    ngettext(
                        n = n,
                        msg1 = "feature",
                        msg2 = "features"
                    )
                ))
            }
            object <- object[keep]
        }
        ## Log transform.
        if (!identical(trans, "identity")) {
            if (isTRUE(summary)) {
                message(
                    "Summary prior to transformation:\n",
                    printString(round(summary(object), digits = 2L))
                )
            }
            message(sprintf("Applying '%s(x + 1)' transformation.", trans))
            fun <- get(
                x = trans,
                envir = asNamespace("base"),
                inherits = FALSE
            )
            object <- fun(object + 1L)
            labels[["x"]] <- paste(trans, labels[["x"]])
        }
        if (isTRUE(summary)) {
            summaryValues <- summary(object)
            message(printString(round(summaryValues, digits = 2L)))
        }
        ## Plot.
        size <- 1L
        linetype <- "solid"
        p <- ggplot(
            data = data.frame(baseMean = object),
            mapping = aes(x = !!sym("baseMean"))
        ) +
            geom_density(
                color = "black",
                fill = NA,
                size = size
            ) +
            scale_x_continuous(breaks = pretty_breaks())
        ## Include the summary distribution lines.
        if (isTRUE(summary)) {
            p <- p +
                geom_vline(
                    mapping = aes(
                        xintercept = summaryValues[["1st Qu."]],
                        color = sprintf(
                            fmt = "1st quantile (%s)",
                            round(summaryValues[["1st Qu."]], digits = 2L)
                        )
                    ),
                    linetype = linetype,
                    size = size
                ) +
                geom_vline(
                    mapping = aes(
                        xintercept = summaryValues[["3rd Qu."]],
                        color = sprintf(
                            fmt = "3rd quantile (%s)",
                            round(summaryValues[["3rd Qu."]], digits = 2L)
                        )
                    ),
                    linetype = linetype,
                    size = size
                ) +
                geom_vline(
                    mapping = aes(
                        xintercept = summaryValues[["Mean"]],
                        color = sprintf(
                            fmt = "mean (%s)",
                            round(summaryValues[["Mean"]], digits = 2L)
                        )
                    ),
                    linetype = linetype,
                    size = size
                ) +
                geom_vline(
                    mapping = aes(
                        xintercept = summaryValues[["Median"]],
                        color = sprintf(
                            fmt = "median (%s)",
                            round(summaryValues[["Median"]], digits = 2L)
                        )
                    ),
                    linetype = linetype,
                    size = size
                )
            ## Color.
            if (is(color, "ScaleDiscrete")) {
                p <- p + color
            }
        }
        ## Labels.
        if (is.list(labels)) {
            p <- p + do.call(what = labs, args = labels)
        }
        ## Return.
        p
    }

formals(`plotBaseMean,numeric`)[["color"]] <- formalsList[["color.discrete"]]



#' @rdname plotBaseMean
#' @export
setMethod(
    f = "plotBaseMean",
    signature = signature("numeric"),
    definition = `plotBaseMean,numeric`
)



## Updated 2019-09-17.
`plotBaseMean,DESeqDataSet` <-  # nolint
    function(object, ...) {
        plotBaseMean(object = rowMeans(counts(object, normalized = TRUE)))
    }



#' @describeIn plotBaseMean Generates row means of normalized counts.
#'   This value corresponds to the `baseMean` column of `DESeqResults`.
#'   Passes to `numeric` method.
#' @export
setMethod(
    f = "plotBaseMean",
    signature = signature("DESeqDataSet"),
    definition = `plotBaseMean,DESeqDataSet`
)



## Updated 2019-09-17.
`plotBaseMean,DESeqResults` <-  # nolint
    function(object, ...) {
        plotBaseMean(object = object[["baseMean"]], ...)
    }



#' @describeIn plotBaseMean Uses `baseMean` column of results.
#'   Passes to `numeric` method.
#' @export
setMethod(
    f = "plotBaseMean",
    signature = signature("DESeqResults"),
    definition = `plotBaseMean,DESeqResults`
)



## Updated 2019-10-15.
`plotBaseMean,DESeqAnalysis` <-  # nolint
    function(object, ...) {
        plotBaseMean(object = as(object, "DESeqDataSet"), ...)
    }



#' @describeIn plotBaseMean Passes to `DESeqDataSet` method.
#' @export
setMethod(
    f = "plotBaseMean",
    signature = signature("DESeqAnalysis"),
    definition = `plotBaseMean,DESeqAnalysis`
)
