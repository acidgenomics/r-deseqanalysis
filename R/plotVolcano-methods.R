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



## Updated 2021-03-15.
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
        limits = list(
            "x" = NULL,
            "y" = c(1e-10, 1e+00)
        ),
        ## NOTE Consider reworking the NULL as TRUE here?
        labels = list(
            title = NULL,
            subtitle = NULL,
            x = "log2 fold change",
            y = "-log10 adj p value"
        ),
        histograms = FALSE
    ) {
        validObject(object)
        baseMeanCol <- "baseMean"
        lfcCol <- "log2FoldChange"
        alphaCol <- ifelse(
            test = isTRUE(isSubset("svalue", names(object))),
            yes = "svalue",
            no = "padj"
        )
        ## Note that `lfcShrink()` doesn't return `stat` column.
        rankCol <- ifelse(
            test = isTRUE(isSubset("stat", names(object))),
            yes = "stat",
            no = lfcCol
        )
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
            is.list(limits),
            areSetEqual(names(limits), c("x", "y")),
            isFlag(histograms)
        )
        direction <- match.arg(direction)
        labels <- matchLabels(
            labels = labels,
            choices = eval(formals()[["labels"]])
        )
        data <- as(object, "DataFrame")
        colnames(data) <- camelCase(colnames(data), strict = TRUE)
        assert(isSubset(c(baseMeanCol, lfcCol, alphaCol), colnames(data)))
        ## Remove genes with NA adjusted P values.
        keep <- which(!is.na(data[[alphaCol]]))
        data <- data[keep, , drop = FALSE]
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
            lfcThreshold = lfcThreshold,
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
        ## Define the limits and correct outliers, if necessary.
        if (is.null(limits[["x"]])) {
            limits[["x"]] <- c(
                min(floor(data[[lfcCol]])),
                max(ceiling(data[[lfcCol]]))
            )
        }
        assert(
            hasLength(limits[["x"]], n = 2L),
            isNegative(limits[["x"]][[1L]]),
            isPositive(limits[["x"]][[2L]])
        )
        ok <- list(
            data[[lfcCol]] >= limits[["x"]][[1L]],
            data[[lfcCol]] <= limits[["x"]][[2L]]
        )
        if (!all(unlist(ok))) {
            n <- sum(!unlist(ok))
            alertWarning(sprintf(
                "%d %s outside x-axis limits of {.var c(%s, %s)}.",
                n,
                ngettext(n = n, msg1 = "point", msg2 = "points"),
                limits[["x"]][[1L]],
                limits[["x"]][[2L]]
            ))
            data[[lfcCol]][!ok[[1L]]] <- limits[["x"]][[1L]]
            data[[lfcCol]][!ok[[2L]]] <- limits[["x"]][[2L]]
        }
        if (is.null(limits[["y"]])) {
            limits[["y"]] <- eval(formals()[["limits"]][["y"]])
        }
        assert(
            hasLength(limits[["y"]], n = 2L),
            isInRange(limits[["y"]][[1L]], lower = 1e-100, upper = 1e-2),
            isInRange(limits[["y"]][[2L]], lower = 1e-1, upper = 1e0)
        )
        ok <- list(
            data[[alphaCol]] >= limits[["y"]][[1L]],
            data[[alphaCol]] <= limits[["y"]][[2L]]
        )
        if (!all(unlist(ok))) {
            n <- sum(!unlist(ok))
            alertWarning(sprintf(
                "%d %s outside y-axis limits of {.var c(%s, %s)}.",
                n,
                ngettext(n = n, msg1 = "point", msg2 = "points"),
                -log10(limits[["y"]][[2L]]),
                -log10(limits[["y"]][[1L]])
            ))
            data[[alphaCol]][!ok[[1L]]] <- limits[["y"]][[1L]]
            data[[alphaCol]][!ok[[2L]]] <- limits[["y"]][[2L]]
        }
        ## Now we are ready to -log10 transform the Y-axis.
        limits[["y"]] <- rev(-log10(limits[["y"]]))
        breaks <- list(
            "x" = pretty_breaks(),
            "y" = pretty_breaks()
        )
        negLogAlphaCol <- camelCase(
            object = paste("neg", "log10", alphaCol),
            strict = TRUE
        )
        data[[negLogAlphaCol]] <- -log10(data[[alphaCol]])
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
            scale_x_continuous(
                breaks = breaks[["x"]],
                limits = limits[["x"]],
                trans = "identity"
            ) +
            scale_y_continuous(
                breaks = breaks[["y"]],
                limits = limits[["y"]],
                trans = "identity"
            ) +
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
        ## Gene text labels.
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
            ## LFC density plot.
            lfcHist <- ggplot(
                data = as_tibble(data, rownames = NULL),
                mapping = aes(x = !!sym(lfcCol))
            ) +
                geom_density(
                    color = NA,
                    fill = pointColor[["nonsignificant"]]
                ) +
                scale_x_continuous(
                    breaks = breaks[["x"]],
                    expand = c(0L, 0L),
                    limits = limits[["x"]]
                ) +
                scale_y_continuous(expand = c(0L, 0L)) +
                labs(x = labels[["x"]], y = NULL) +
                guides(fill = FALSE) +
                theme(
                    axis.line.y = element_blank(),
                    axis.text.y = element_blank(),
                    axis.ticks.y = element_blank()
                )
            ## P value density plot.
            pvalueHist <- ggplot(
                data = as_tibble(data, rownames = NULL),
                mapping = aes(x = !!sym(negLogAlphaCol))
            ) +
                geom_density(
                    color = NA,
                    fill = pointColor[["nonsignificant"]]
                ) +
                scale_x_continuous(
                    breaks = breaks[["y"]],
                    expand = c(0L, 0L),
                    limits = limits[["y"]]
                ) +
                scale_y_continuous(expand = c(0L, 0L)) +
                labs(x = labels[["y"]], y = NULL) +
                guides(fill = FALSE) +
                theme(
                    axis.line.y = element_blank(),
                    axis.text.y = element_blank(),
                    axis.ticks.y = element_blank()
                )
            ## Coordinates are relative to lower left corner.
            ggdraw() +
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
