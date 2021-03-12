## NOTE Consider gating LFC at +/- 10 here by default.



#' @name plotMA
#' @inherit AcidGenerics::plotMA
#' @author Michael Steinbaugh, Rory Kirchner
#' @note Updated 2021-03-03.
#'
#' @details
#' An MA plot is an application of a Bland–Altman plot for visual
#' representation of genomic data. The plot visualizes the differences between
#' measurements taken in two samples, by transforming the data onto
#' M (log ratio) and A (mean average) scales, then plotting these values.
#'
#' @section plotMA2 aliases:
#'
#' Aliased methods for original [DESeq2::plotMA()] S4 methods, which us
#' geneplotter instead of ggplot2. I prefer using ggplot2 instead, so the
#' primary methods defined here in the package mask DESeq2.
#'
#' @inheritParams AcidRoxygen::params
#' @inheritParams params
#' @param ... Additional arguments.
#'
#' @seealso [DESeq2::plotMA()].
#'
#' @return `ggplot`.
#'
#' @examples
#' data(deseq)
#'
#' ## Get genes from DESeqDataSet.
#' dds <- as(deseq, "DESeqDataSet")
#' genes <- head(rownames(dds))
#' print(genes)
#'
#' ## DESeqAnalysis ====
#' plotMA(deseq, i = 1L)
#'
#' ## Customize the colors.
#' plotMA(
#'     object = deseq,
#'     i = 1L,
#'     pointColor = c(
#'         downregulated = "red",
#'         nonsignificant = "black",
#'         upregulated = "green"
#'     )
#' )
#'
#' ## Directional support (up or down).
#' plotMA(deseq, i = 1L, direction = "up", ntop = 5L)
#' plotMA(deseq, i = 1L, direction = "down", ntop = 5L)
#'
#' ## Label genes manually.
#' ## Note that either gene IDs or names (symbols) are supported.
#' plotMA(deseq, i = 1L, genes = genes)
NULL



## Updated 2021-03-03.
`plotMA,DESeqResults` <-  # nolint
    function(
        object,
        alphaThreshold = NULL,
        lfcThreshold = NULL,
        baseMeanThreshold = NULL,
        genes = NULL,
        gene2symbol = NULL,
        ntop = 0L,
        direction = c("both", "up", "down"),
        pointColor = c(
            downregulated = AcidPlots::lightPalette[["purple"]],
            upregulated = AcidPlots::lightPalette[["orange"]],
            nonsignificant = AcidPlots::lightPalette[["gray"]]
        ),
        pointSize = 2L,
        pointAlpha = 0.8,
        ## NOTE Consider reworking the NULL as TRUE here?
        labels = list(
            title = NULL,
            subtitle = NULL,
            x = "mean expression across all samples",
            y = "log2 fold change"
        )
    ) {
        validObject(object)
        baseMeanCol <- "baseMean"
        lfcCol <- "log2FoldChange"
        if ("svalue" %in% names(object)) {
            alphaCol <- "svalue"  # nocov
        } else {
            alphaCol <- "padj"
        }
        ## Note that `lfcShrink()` doesn't return `stat` column.
        if ("stat" %in% names(object)) {
            rankCol <- "stat"
        } else {
            rankCol <- lfcCol
        }
        if (is.null(alphaThreshold)) {
            alphaThreshold <- alphaThreshold(object)
        }
        if (is.null(lfcThreshold)) {
            lfcThreshold <- lfcThreshold(object)
        }
        if (is.null(baseMeanThreshold)) {
            baseMeanThreshold <- baseMeanThreshold(object)
        }
        ## We're applying log10 transformation on axis, so gate the minimum.
        if (baseMeanThreshold < 1L) {
            baseMeanThreshold <- 1L
        }
        lfcShrinkType <- lfcShrinkType(object)
        assert(
            isAlpha(alphaThreshold),
            isNumber(lfcThreshold),
            isNonNegative(lfcThreshold),
            isString(lfcShrinkType),
            isNumber(baseMeanThreshold),
            isPositive(baseMeanThreshold),
            isAny(genes, classes = c("character", "NULL")),
            isAny(gene2symbol, classes = c("Gene2Symbol", "NULL")),
            isCharacter(pointColor),
            areSetEqual(
                x = names(pointColor),
                y = c("downregulated", "nonsignificant", "upregulated")
            ),
            isNumber(pointSize),
            isNonNegative(pointSize),
            isPercentage(pointAlpha),
            isInt(ntop),
            isNonNegative(ntop)
        )
        direction <- match.arg(direction)
        labels <- matchLabels(
            labels = labels,
            choices = eval(formals()[["labels"]])
        )
        ## Genes or ntop, but not both.
        if (!is.null(genes) && ntop > 0L) {
            stop("Specify either 'genes' or 'ntop'.")
        }
        data <- as(object, "DataFrame")
        data <- camelCase(data, strict = TRUE)
        assert(isSubset(
            x = c(baseMeanCol, lfcCol, rankCol, alphaCol),
            y = colnames(data)
        ))
        ## Remove genes with very low expression.
        keep <- which(data[[baseMeanCol]] >= baseMeanThreshold)
        data <- data[keep, , drop = FALSE]
        data[["rankScore"]] <- abs(data[[rankCol]])
        data <- data[
            order(data[["rankScore"]], decreasing = TRUE), , drop = FALSE
        ]
        data[["rank"]] <- seq_len(nrow(data))
        data <- .addIsDegCol(
            data = data,
            alphaCol = alphaCol,
            alphaThreshold = alphaThreshold,
            lfcCol = lfcCol,
            lfcThreshold = lfcThreshold,
            baseMeanCol = baseMeanCol,
            baseMeanThreshold = baseMeanThreshold
        )
        assert(isSubset(c("isDeg", "rank", "rankScore"), colnames(data)))
        ## Apply directional filtering, if desired.
        switch(
            EXPR = direction,
            "up" = {
                keep <- which(data[[lfcCol]] > 0L)
                data <- data[keep, , drop = FALSE]
            },
            "down" = {
                keep <- which(data[[lfcCol]] < 0L)
                data <- data[keep, , drop = FALSE]
            }
        )
        ## Check for no genes passing cutoffs and early return.
        if (!hasRows(data)) {
            alertWarning("No genes passed cutoffs.")
            return(invisible(NULL))
        }
        ## MA plot.
        log10BaseMean <- log10(data[["baseMean"]])
        floor <- min(floor(log10BaseMean))
        ceiling <- max(ceiling(log10BaseMean))
        xBreaks <- 10L ^ seq(from = floor, to = ceiling, by = 1L)
        p <- ggplot(
            data = as_tibble(data, rownames = NULL),
            mapping = aes(
                x = !!sym("baseMean"),
                y = !!sym(lfcCol),
                color = !!sym("isDeg")
            )
        ) +
            geom_hline(
                yintercept = 0L,
                size = 0.5,
                color = pointColor[["nonsignificant"]]
            ) +
            geom_point(
                alpha = pointAlpha,
                size = pointSize,
                stroke = 0L
            ) +
            scale_x_continuous(
                breaks = xBreaks,
                limits = c(baseMeanThreshold, NA),
                trans = "log10"
            ) +
            scale_y_continuous(breaks = pretty_breaks()) +
            annotation_logticks(sides = "b") +
            guides(color = FALSE)
        ## Labels.
        if (is.null(labels[["title"]])) {
            labels[["title"]] <- tryCatch(
                expr = contrastName(object),
                error = function(e) NULL
            )
        }
        if (is.null(labels[["subtitle"]])) {
            labels[["subtitle"]] <- .thresholdLabel(
                object = object,
                direction = direction,
                alphaThreshold = alphaThreshold,
                lfcShrinkType = lfcShrinkType,
                lfcThreshold = lfcThreshold,
                baseMeanThreshold = baseMeanThreshold
            )
        }
        p <- p + do.call(what = labs, args = labels)
        ## Color the significant points.
        ## Note that we're using direction-specific coloring by default.
        if (isCharacter(pointColor)) {
            p <- p +
                scale_color_manual(
                    values = c(
                        "-1" = pointColor[["downregulated"]],
                        "0" = pointColor[["nonsignificant"]],
                        "1" = pointColor[["upregulated"]]
                    )
                )
        }
        ## Gene text labels.
        ## Get the genes to visualize when `ntop` is declared.
        if (isTRUE(ntop > 0L)) {
            assert(
                hasRownames(data),
                isSubset("rank", colnames(data)),
                identical(data[["rank"]], sort(data[["rank"]]))
            )
            ## Since we know the data is arranged by rank, simply take the head.
            genes <- head(rownames(data), n = ntop)
        }
        ## Visualize specific genes on the plot, if desired.
        if (!is.null(genes)) {
            validObject(gene2symbol)
            assert(matchesGene2Symbol(
                x = object,
                genes = genes,
                gene2symbol = gene2symbol
            ))
            ## Map the user-defined `genes` to `gene2symbol` rownames.
            ## We're using this to match back to the `DESeqResults` object.
            rownames <- mapGenesToRownames(object = gene2symbol, genes = genes)
            gene2symbol <- as(gene2symbol, "DataFrame")
            gene2symbol[["rowname"]] <- rownames(gene2symbol)
            ## Prepare the label data tibble.
            keep <- na.omit(match(x = rownames, table = rownames(data)))
            assert(hasLength(keep))
            labelData <- data[keep, , drop = FALSE]
            labelData[["rowname"]] <- rownames(labelData)
            labelData <- leftJoin(labelData, gene2symbol, by = "rowname")
            p <- p +
                acid_geom_label_repel(
                    data = as_tibble(labelData, rownames = NULL),
                    mapping = aes(
                        x = !!sym("baseMean"),
                        y = !!sym(lfcCol),
                        label = !!sym("geneName")
                    )
                )
        }
        p
    }



#' @rdname plotMA
#' @export
setMethod(
    f = "plotMA",
    signature = signature("DESeqResults"),
    definition = `plotMA,DESeqResults`
)



## Updated 2020-08-05.
`plotMA,DESeqAnalysis` <-  # nolint
    function(object, i, ...) {
        plotMA(
            object = results(object, i = i),
            gene2symbol = tryCatch(
                expr = suppressMessages({
                    Gene2Symbol(as(object, "DESeqDataSet"))
                }),
                error = function(e) NULL
            ),
            alphaThreshold = alphaThreshold(object),
            lfcThreshold = lfcThreshold(object),
            baseMeanThreshold = baseMeanThreshold(object),
            ...
        )
    }



#' @describeIn plotMA Passes to `DESeqResults` method, with `gene2symbol`
#'   argument automatically defined.
#' @export
setMethod(
    f = "plotMA",
    signature = signature("DESeqAnalysis"),
    definition = `plotMA,DESeqAnalysis`
)



## Aliases =====================================================================
## Soft deprecated, since this is used in bcbioRNASeq F1000 paper.
#' @rdname plotMA
#' @usage NULL
#' @export
plotMeanAverage <- function(...) {
    ## > .Deprecated("plotMA")
    plotMA(...)
}
