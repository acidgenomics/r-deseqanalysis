#' Plot base mean distribution
#'
#' The base mean is the mean of normalized counts of all samples, normalizing
#' for sequencing depth.
#'
#' @name plotBaseMean
#' @note Updated 2019-09-10.
#'
#' @inheritParams acidroxygen::params
#' @inheritParams params
#'
#' @seealso
#' - https://support.bioconductor.org/p/75244/
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



## Updated 2019-09-10.
.plotBaseMean <- function(
    x,
    nonzero = TRUE,
    trans = c("log10", "log2", "identity")
) {
    assert(
        is.numeric(x),
        isFlag(nonzero)
    )
    trans <- match.arg(trans)
    xLab <- "average expression across all samples"
    ## Drop zero values prior to plot.
    if (isTRUE(nonzero)) {
        keep <- x > 0L
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
        x <- x[keep]
    }
    ## Log transform.
    if (!identical(trans, "identity")) {
        message(printString(round(summary(x), digits = 2L)))
        message(sprintf("Applying '%s(x + 1)' transformation.", trans))
        fun <- get(x = trans, envir = asNamespace("base"), inherits = FALSE)
        x <- fun(x + 1L)
        xLab <- paste(trans, xLab)
    }
    summary <- summary(x)
    message(printString(round(summary, digits = 2L)))
    ## Plot.
    size <- 1L
    linetype <- "solid"
    ggplot(
        data = data.frame(baseMean = x),
        mapping = aes(
            x = !!sym("baseMean")
        )
    ) +
        geom_density(
            colour = "black",
            fill = NA,
            size = size
        ) +
        scale_x_continuous(breaks = pretty_breaks()) +
        geom_vline(
            mapping = aes(
                xintercept = summary[["1st Qu."]],
                colour = sprintf(
                    fmt = "1st quantile (%s)",
                    round(summary[["1st Qu."]], digits = 2L)
                )
            ),
            linetype = linetype,
            size = size
        ) +
        geom_vline(
            mapping = aes(
                xintercept = summary[["3rd Qu."]],
                colour = sprintf(
                    fmt = "3rd quantile (%s)",
                    round(summary[["3rd Qu."]], digits = 2L)
                )
            ),
            linetype = linetype,
            size = size
        ) +
        geom_vline(
            mapping = aes(
                xintercept = summary[["Mean"]],
                colour = sprintf(
                    fmt = "mean (%s)",
                    round(summary[["Mean"]], digits = 2L)
                )
            ),
            linetype = linetype,
            size = size
        ) +
        geom_vline(
            mapping = aes(
                xintercept = summary[["Median"]],
                colour = sprintf(
                    fmt = "median (%s)",
                    round(summary[["Median"]], digits = 2L)
                )
            ),
            linetype = linetype,
            size = size
        ) +
        scale_colour_synesthesia_d(name = "summary") +
        labs(
            title = "Base mean distribution",
            x = xLab
        )
}



## Updated 2019-09-10.
`plotBaseMean,DESeqDataSet` <-  # nolint
    function(
        object,
        nonzero,
        trans
    ) {
        trans <- match.arg(trans)
        x <- rowMeans(counts(object, normalized = TRUE))
        .plotBaseMean(
            x = x,
            nonzero = nonzero,
            trans = trans
        )
    }

args <- c("nonzero", "trans")
formals(`plotBaseMean,DESeqDataSet`)[args] <- formals(.plotBaseMean)[args]



#' @rdname plotBaseMean
#' @export
setMethod(
    f = "plotBaseMean",
    signature = signature("DESeqDataSet"),
    definition = `plotBaseMean,DESeqDataSet`
)



## Updated 2019-07-30.
`plotBaseMean,DESeqResults` <-  # nolint
    function(
        object,
        nonzero,
        trans
    ) {
        trans <- match.arg(trans)
        x <- object[["baseMean"]]
        .plotBaseMean(
            x = x,
            nonzero = nonzero,
            trans = trans
        )
    }

formals(`plotBaseMean,DESeqResults`) <- formals(`plotBaseMean,DESeqDataSet`)



#' @rdname plotBaseMean
#' @export
setMethod(
    f = "plotBaseMean",
    signature = signature("DESeqResults"),
    definition = `plotBaseMean,DESeqResults`
)



## Updated 2019-09-10.
`plotBaseMean,DESeqAnalysis` <-  # nolint
    function(
        object,
        nonzero,
        trans
    ) {
        trans <- match.arg(trans)
        dds <- as(object, "DESeqDataSet")
        plotBaseMean(
            object = dds,
            nonzero = nonzero,
            trans = trans
        )
    }

formals(`plotBaseMean,DESeqAnalysis`) <- formals(`plotBaseMean,DESeqDataSet`)



#' @rdname plotBaseMean
#' @export
setMethod(
    f = "plotBaseMean",
    signature = signature("DESeqAnalysis"),
    definition = `plotBaseMean,DESeqAnalysis`
)
