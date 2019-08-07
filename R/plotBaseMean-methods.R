#' Plot base mean distribution
#'
#' The base mean is the mean of normalized counts of all samples, normalizing
#' for sequencing depth.
#'
#' @name plotBaseMean
#' @note Updated 2019-07-30.
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



## Updated 2019-07-30.
.plotBaseMean <- function(baseMean) {
    assert(is.numeric(baseMean))
    ## Drop zero values prior to plot.
    baseMean <- baseMean[baseMean > 0L]
    data <- data.frame(baseMean = baseMean)
    summary <- summary(baseMean)
    message(printString(round(summary)))
    size <- 1L
    linetype <- "solid"
    ggplot(
        data = data,
        mapping = aes(
            x = !!sym("baseMean")
        )
    ) +
        geom_density(
            colour = "black",
            fill = NA,
            size = size
        ) +
        scale_x_continuous(
            trans = "log10",
            breaks = log_breaks(n = Inf, base = 2L)
        ) +
        geom_vline(
            mapping = aes(
                xintercept = summary[["1st Qu."]],
                colour = "1st quantile"
            ),
            linetype = linetype,
            size = size
        ) +
        geom_vline(
            mapping = aes(
                xintercept = summary[["3rd Qu."]],
                colour = "3rd quantile"
            ),
            linetype = linetype,
            size = size
        ) +
        geom_vline(
            mapping = aes(
                xintercept = summary[["Mean"]],
                colour = "mean"
            ),
            linetype = linetype,
            size = size
        ) +
        geom_vline(
            mapping = aes(
                xintercept = summary[["Median"]],
                colour = "median"
            ),
            linetype = linetype,
            size = size
        ) +
        scale_colour_synesthesia_d(name = "summary") +
        labs(
            title = "Base mean distribution",
            x = "average expression across all samples"
        )
}



## Updated 2019-07-30.
`plotBaseMean,DESeqDataSet` <-  # nolint
    function(object) {
        baseMean <- rowMeans(counts(object, normalized = TRUE))
        .plotBaseMean(baseMean)
    }



#' @rdname plotBaseMean
#' @export
setMethod(
    f = "plotBaseMean",
    signature = signature("DESeqDataSet"),
    definition = `plotBaseMean,DESeqDataSet`
)



## Updated 2019-07-30.
`plotBaseMean,DESeqResults` <-  # nolint
    function(object) {
        baseMean <- object[["baseMean"]]
        .plotBaseMean(baseMean)
    }



#' @rdname plotBaseMean
#' @export
setMethod(
    f = "plotBaseMean",
    signature = signature("DESeqResults"),
    definition = `plotBaseMean,DESeqResults`
)



## Updated 2019-07-30.
`plotBaseMean,DESeqAnalysis` <-  # nolint
    function(object) {
        dds <- as(object, "DESeqDataSet")
        plotBaseMean(dds)
    }



#' @rdname plotBaseMean
#' @export
setMethod(
    f = "plotBaseMean",
    signature = signature("DESeqAnalysis"),
    definition = `plotBaseMean,DESeqAnalysis`
)
