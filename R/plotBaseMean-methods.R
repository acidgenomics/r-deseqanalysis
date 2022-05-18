#' @name plotBaseMean
#' @inherit AcidGenerics::plotBaseMean
#' @note Updated 2022-05-17.
#'
#' @inheritParams AcidRoxygen::params
#' @inheritParams params
#'
#' @param nonzero `logical(1)`.
#' Remove zero-count features (genes).
#'
#' @param summary `logical(1)`.
#' Include distribution summary statistics as lines on the plot.
#'
#' @seealso
#' - https://support.bioconductor.org/p/75244/
#' - `summary()`.
#'
#' @return `ggplot`.
#'
#' @examples
#' data(deseq)
#'
#' ## DESeqAnalysis ====
#' plotBaseMean(deseq)
NULL



## Updated 2022-05-18.
`plotBaseMean,DESeqAnalysis` <- # nolint
    function(object,
             nonzero = TRUE,
             trans = c("log10", "log2", "identity"),
             summary = TRUE,
             labels = list(
                 "title" = "Base mean distribution",
                 "subtitle" = NULL
             )) {
        plotBaseMean(
            object = as.DESeqDataSet(object),
            nonzero = nonzero,
            trans = match.arg(trans),
            summary = summary,
            labels = labels
        )
    }



## Updated 2022-05-18.
`plotBaseMean,DESeqDataSet` <- # nolint
    function(object,
             nonzero = TRUE,
             trans = c("log10", "log2", "identity"),
             summary = TRUE,
             labels = list(
                 "title" = "Base mean distribution",
                 "subtitle" = NULL
             )) {
        `plotBaseMean,numeric`(
            object = rowMeans(counts(object, normalized = TRUE)),
            nonzero = nonzero,
            trans = match.arg(trans),
            summary = summary,
            labels = labels
        )
    }



## Updated 2022-05-18.
`plotBaseMean,DESeqResults` <- # nolint
    function(object,
             nonzero = TRUE,
             trans = c("log10", "log2", "identity"),
             summary = TRUE,
             labels = list(
                 "title" = "Base mean distribution",
                 "subtitle" = NULL
             )) {
        `plotBaseMean,numeric`(
            object = object[["baseMean"]],
            nonzero = nonzero,
            trans = match.arg(trans),
            summary = summary,
            labels = labels
        )
    }



## Updated 2019-10-15.
`plotBaseMean,numeric` <- # nolint
    function(object,
             nonzero = TRUE,
             trans = c("log10", "log2", "identity"),
             summary = TRUE,
             labels = list(
                 "title" = "Base mean distribution",
                 "subtitle" = NULL
             )) {
        assert(
            is.numeric(object),
            isFlag(nonzero),
            isFlag(summary)
        )
        trans <- match.arg(trans)
        labels <- matchLabels(labels)
        labels[["x"]] <- "average expression across all samples"
        labels[["y"]] <- "density"
        ## Drop zero values prior to plot.
        if (isTRUE(nonzero)) {
            keep <- object > 0L
            ## Inform the user about how many zero count features were dropped.
            if (!all(keep)) {
                n <- sum(!keep, na.rm = TRUE)
                alertInfo(sprintf(
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
                alertInfo(paste(
                    "Summary prior to transformation:",
                    printString(round(summary(object), digits = 2L)),
                    sep = "\n"
                ))
            }
            alert(sprintf("Applying '%s(x + 1)' transformation.", trans))
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
            alertInfo(paste(
                "Summary after transformation:",
                printString(round(summaryValues, digits = 2L)),
                sep = "\n"
            ))
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
            ## Color palette.
            p <- p + autoDiscreteColorScale()
        }
        ## Labels.
        labels[["color"]] <- "summary"
        p <- p + do.call(what = labs, args = labels)
        ## Return.
        p
    }



#' @describeIn plotBaseMean Passes to `DESeqDataSet` method.
#' @export
setMethod(
    f = "plotBaseMean",
    signature = signature(object = "DESeqAnalysis"),
    definition = `plotBaseMean,DESeqAnalysis`
)

#' @describeIn plotBaseMean Generates row means of normalized counts.
#' This value corresponds to the `baseMean` column of `DESeqResults`.
#' @export
setMethod(
    f = "plotBaseMean",
    signature = signature(object = "DESeqDataSet"),
    definition = `plotBaseMean,DESeqDataSet`
)

#' @describeIn plotBaseMean Uses `baseMean` column of results.
#' @export
setMethod(
    f = "plotBaseMean",
    signature = signature(object = "DESeqResults"),
    definition = `plotBaseMean,DESeqResults`
)
