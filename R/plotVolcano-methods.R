## FIXME Improve the support for gating axes.
## NOTE Consider gating LFC at +/- 10 here by default.



#' @name plotVolcano
#' @author Michael Steinbaugh, John Hutchinson, Lorena Pantano
#' @inherit AcidGenerics::plotVolcano
#' @note Updated 2021-03-15.
#'
#' @inheritParams AcidRoxygen::params
#' @inheritParams params
#' @param ylim `numeric(1)`.
#'   Upper boundary limit for y-axis. Helps preserve dynamic range for gene sets
#'   containing highly significant P values (e.g. `1e-100`).
#' @param histograms `logical(1)`.
#'   Show LFC and P value histograms.
#' @param ... Additional arguments.
#'
#' @seealso `CHBUtils::volcano_density_plot()`.
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
#' plotVolcano(deseq, i = 1L)
#'
#' ## Customize the colors.
#' plotVolcano(
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
#' plotVolcano(
#'     object = deseq,
#'     i = 1L,
#'     direction = "up",
#'     ntop = 5L
#' )
#' plotVolcano(
#'     object = deseq,
#'     i = 1L,
#'     direction = "down",
#'     ntop = 5L
#' )
#'
#' ## Label genes manually.
#' ## Note that either gene IDs or names (symbols) are supported.
#' plotVolcano(deseq, i = 1L, genes = genes)
NULL



## Updated 2021-03-15.
`plotVolcano,DESeqAnalysis` <-  # nolint
    function(
        object,
        i,
        alphaThreshold = NULL,
        lfcThreshold = NULL,
        baseMeanThreshold = NULL,
        ...
    ) {
        plotVolcano(
            object = results(object, i = i),
            gene2symbol = tryCatch(
                expr = suppressMessages({
                    Gene2Symbol(as(object, "DESeqDataSet"))
                }),
                error = function(e) NULL
            ),
            alphaThreshold = ifelse(
                test = is.null(alphaThreshold),
                yes = alphaThreshold(object),
                no = alphaThreshold
            ),
            lfcThreshold = ifelse(
                test = is.null(lfcThreshold),
                yes = lfcThreshold(object),
                no = lfcThreshold
            ),
            baseMeanThreshold = ifelse(
                test = is.null(baseMeanThreshold),
                yes = baseMeanThreshold(object),
                no = baseMeanThreshold
            ),
            ...
        )
    }



## Updated 2020-08-04.
`plotVolcano,DESeqResults` <-  # nolint
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
        ylim = 1e-10,
        ## NOTE Consider reworking the NULL as TRUE here?
        labels = list(
            title = NULL,
            subtitle = NULL,
            x = "log2 fold change",
            y = "-log10 adj p value"
        ),
        histograms = FALSE,
        ## NOTE Consider removing this option?
        return = c("ggplot", "DataFrame")
    ) {
        validObject(object)
        baseMeanCol <- "baseMean"
        lfcCol <- "log2FoldChange"
        if ("svalue" %in% names(object)) {
            alphaCol <- "svalue"  # nocov
        } else {
            alphaCol <- "padj"
        }
        negLogAlphaCol <- camelCase(
            object = paste("neg", "log10", alphaCol),
            strict = TRUE
        )
        ## Note that `lfcShrink()` doesn't return `stat` column.
        if ("stat" %in% names(object)) {
            rankCol <- "stat"
        } else {
            rankCol <- negLogAlphaCol
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
        lfcShrinkType <- lfcShrinkType(object)
        assert(
            isAlpha(alphaThreshold),
            isNumber(lfcThreshold),
            isNonNegative(lfcThreshold),
            isString(lfcShrinkType),
            isNumber(baseMeanThreshold),
            isNonNegative(baseMeanThreshold),
            isInt(ntop),
            isNonNegative(ntop),
            isCharacter(pointColor),
            areSetEqual(
                x = names(pointColor),
                y = c("downregulated", "nonsignificant", "upregulated")
            ),
            isNumber(pointSize),
            isNonNegative(pointSize),
            isPercentage(pointAlpha),
            isNumber(ylim),
            isInRange(ylim, lower = 1e-100, upper = 1e-3),
            isFlag(histograms)
        )
        direction <- match.arg(direction)
        labels <- matchLabels(
            labels = labels,
            choices = eval(formals()[["labels"]])
        )
        return <- match.arg(return)
        if (isTRUE(histograms)) {
            assert(identical(return, "ggplot"))
        }
        data <- as(object, "DataFrame")
        data <- camelCase(data, strict = TRUE)
        assert(isSubset(c(baseMeanCol, lfcCol, alphaCol), colnames(data)))
        ## Remove genes with NA adjusted P values.
        keep <- which(!is.na(data[[alphaCol]]))
        data <- data[keep, , drop = FALSE]
        keep <- which(data[[baseMeanCol]] >= baseMeanThreshold)
        data <- data[keep, , drop = FALSE]
        ## Negative log10 transform the test values. Add `ylim` here to prevent
        ## `Inf` values resulting from log transformation. This will also define
        ## the upper bound of the y-axis. Then calculate the rank score, which
        ## is used for `ntop`.
        data[[negLogAlphaCol]] <- -log10(data[[alphaCol]] + ylim)
        data[["rankScore"]] <- abs(data[[rankCol]])
        data <- data[
            order(data[["rankScore"]], decreasing = TRUE), , drop = FALSE
        ]
        data[["rank"]] <- seq_len(nrow(data))
        data <- .addIsDegCol(
            data = data,
            alphaCol = alphaCol,
            alphaThreshold = alphaThreshold,
            lfcThreshold = lfcThreshold,
            baseMeanThreshold = baseMeanThreshold
        )
        assert(isSubset(c("isDeg", "rank", "rankScore"), colnames(data)))
        ## Apply directional filtering, if desired.
        if (direction == "up") {
            keep <- which(data[[lfcCol]] > 0L)
            data <- data[keep, , drop = FALSE]
        } else if (direction == "down") {
            keep <- which(data[[lfcCol]] < 0L)
            data <- data[keep, , drop = FALSE]
        }
        ## Early return the data, if desired.
        if (identical(return, "DataFrame")) {
            return(data)
        }
        ## LFC density ---------------------------------------------------------
        lfcHist <- ggplot(
            data = as_tibble(data, rownames = NULL),
            mapping = aes(x = !!sym(lfcCol))
        ) +
            geom_density(
                color = NA,
                fill = pointColor[["nonsignificant"]]
            ) +
            scale_x_continuous(
                breaks = pretty_breaks(),
                expand = c(0L, 0L)
            ) +
            scale_y_continuous(expand = c(0L, 0L)) +
            labs(
                x = "log2 fold change",
                y = NULL
            ) +
            guides(fill = FALSE) +
            theme(
                axis.line.y = element_blank(),
                axis.text.y = element_blank(),
                axis.ticks.y = element_blank()
            )
        ## P value density -----------------------------------------------------
        pvalueHist <- ggplot(
            data = as_tibble(data, rownames = NULL),
            mapping = aes(x = !!sym(negLogAlphaCol))
        ) +
            geom_density(
                color = NA,
                fill = pointColor[["nonsignificant"]]
            ) +
            scale_x_continuous(
                breaks = pretty_breaks(),
                expand = c(0L, 0L)
            ) +
            scale_y_continuous(expand = c(0L, 0L)) +
            labs(
                x = "-log10 adj p value",
                y = NULL
            ) +
            guides(fill = FALSE) +
            theme(
                axis.line.y = element_blank(),
                axis.text.y = element_blank(),
                axis.ticks.y = element_blank()
            )
        ## Volcano plot --------------------------------------------------------
        p <- ggplot(
            data = as_tibble(data, rownames = NULL),
            mapping = aes(
                x = !!sym(lfcCol),
                y = !!sym(negLogAlphaCol),
                color = !!sym("isDeg")
            )
        ) +
            geom_vline(
                xintercept = 0L,
                size = 0.5,
                color = pointColor[["nonsignificant"]]
            ) +
            geom_point(
                alpha = pointAlpha,
                size = pointSize,
                stroke = 0L
            ) +
            scale_x_continuous(breaks = pretty_breaks()) +
            scale_y_continuous(breaks = pretty_breaks()) +
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
        ## Gene text labels ----------------------------------------------------
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
        ## Return --------------------------------------------------------------
        if (isTRUE(histograms)) {
            ggdraw() +
                ## Coordinates are relative to lower left corner
                draw_plot(
                    plot = p,
                    x = 0L, y = 0.2,
                    width = 1L, height = 0.8
                ) +
                draw_plot(
                    plot = lfcHist,
                    x = 0L, y = 0L,
                    width = 0.5, height = 0.2
                ) +
                draw_plot(
                    plot = pvalueHist,
                    x = 0.5, y = 0L,
                    width = 0.5, height = 0.2
                )
        } else {
            p
        }
    }



#' @describeIn plotVolcano Passes to `DESeqResults` method, with `gene2symbol`
#'   argument automatically defined.
#' @export
setMethod(
    f = "plotVolcano",
    signature = signature("DESeqAnalysis"),
    definition = `plotVolcano,DESeqAnalysis`
)



#' @rdname plotVolcano
#' @export
setMethod(
    f = "plotVolcano",
    signature = signature("DESeqResults"),
    definition = `plotVolcano,DESeqResults`
)
