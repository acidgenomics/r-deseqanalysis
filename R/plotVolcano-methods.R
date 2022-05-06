#' @name plotVolcano
#' @author Michael Steinbaugh, John Hutchinson, Lorena Pantano
#' @inherit AcidGenerics::plotVolcano
#' @note Updated 2022-04-15.
#'
#' @inheritParams AcidRoxygen::params
#' @inheritParams params
#' @param ... Additional arguments.
#'
#' @param histograms `logical(1)`.
#' Show LFC and P value histograms.
#'
#' @seealso `CHBUtils::volcano_density_plot()`.
#'
#' @examples
#' data(deseq)
#'
#' ## Get genes from DESeqDataSet.
#' dds <- as.DESeqDataSet(deseq)
#' genes <- head(rownames(dds))
#' print(genes)
#'
#' ## DESeqAnalysis ====
#' object <- deseq
#' plotVolcano(object, i = 1L)
#'
#' ## Customize the colors.
#' plotVolcano(
#'     object = object,
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
#'     object = object,
#'     i = 1L,
#'     direction = "up",
#'     ntop = 5L
#' )
#' plotVolcano(
#'     object = object,
#'     i = 1L,
#'     direction = "down",
#'     ntop = 5L
#' )
#'
#' ## Label genes manually.
#' ## Note that either gene IDs or names (symbols) are supported.
#' plotVolcano(object, i = 1L, genes = genes)
NULL



## Updated 2021-07-27.
`plotVolcano,DESeqAnalysis` <- # nolint
    function(object,
             i,
             alphaThreshold = NULL,
             baseMeanThreshold = NULL,
             lfcThreshold = NULL,
             genes = NULL,
             ntop = 0L,
             ...) {
        validObject(object)
        assert(
            isAny(genes, classes = c("character", "NULL")),
            isInt(ntop),
            isNonNegative(ntop)
        )
        dds <- as(object, "DESeqDataSet")
        res <- results(object, i = i)
        assert(identical(rownames(dds), rownames(res)))
        if (isCharacter(genes)) {
            genes <- mapGenesToSymbols(
                object = dds,
                genes = genes,
                strict = TRUE
            )
        }
        if (isCharacter(genes) || isTRUE(isPositive(ntop))) {
            dds <- convertGenesToSymbols(dds)
            if (isCharacter(genes)) {
                assert(isSubset(genes, rownames(dds)))
            }
            rownames(res) <- rownames(dds)
        }
        plotVolcano(
            object = res,
            alphaThreshold = ifelse(
                test = is.null(alphaThreshold),
                yes = alphaThreshold(object),
                no = alphaThreshold
            ),
            baseMeanThreshold = ifelse(
                test = is.null(baseMeanThreshold),
                yes = baseMeanThreshold(object),
                no = baseMeanThreshold
            ),
            lfcThreshold = ifelse(
                test = is.null(lfcThreshold),
                yes = lfcThreshold(object),
                no = lfcThreshold
            ),
            genes = genes,
            ntop = ntop,
            ...
        )
    }



## Updated 2021-08-09.
`plotVolcano,DESeqResults` <- # nolint
    function(object,
             direction = c("both", "up", "down"),
             alphaThreshold = NULL,
             baseMeanThreshold = NULL,
             lfcThreshold = NULL,
             genes = NULL,
             ntop = 0L,
             pointColor = c(
                 "downregulated" = AcidPlots::lightPalette[["purple"]],
                 "upregulated" = AcidPlots::lightPalette[["orange"]],
                 "nonsignificant" = AcidPlots::lightPalette[["gray"]]
             ),
             pointSize = 2L,
             pointAlpha = 0.8,
             limits = list(
                 "x" = NULL,
                 "y" = c(1e-10, 1L)
             ),
             labels = list(
                 "title" = TRUE,
                 "subtitle" = NULL
             ),
             histograms = FALSE) {
        validObject(object)
        if (is.null(alphaThreshold)) {
            alphaThreshold <- alphaThreshold(object)
        }
        if (is.null(baseMeanThreshold)) {
            baseMeanThreshold <- baseMeanThreshold(object)
        }
        if (is.null(lfcThreshold)) {
            lfcThreshold <- lfcThreshold(object)
        }
        lfcShrinkType <- lfcShrinkType(object)
        assert(
            isAlpha(alphaThreshold),
            isNumber(baseMeanThreshold),
            isNonNegative(baseMeanThreshold),
            isNumber(lfcThreshold),
            isNonNegative(lfcThreshold),
            isString(lfcShrinkType),
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
        labels <- matchLabels(labels)
        assert(
            !(isCharacter(genes) && isTRUE(isPositive(ntop))),
            msg = "Specify either 'genes' or 'ntop'."
        )
        data <- .prepareResultsForPlot(
            object = object,
            direction = direction,
            alphaThreshold = alphaThreshold,
            baseMeanThreshold = baseMeanThreshold,
            lfcThreshold = lfcThreshold
        )
        if (!hasRows(data)) {
            return(invisible(NULL))
        }
        assert(isSubset(
            x = c("baseMeanCol", "isDegCol", "lfcCol"),
            y = names(metadata(data))
        ))
        alphaCol <- metadata(data)[["alphaCol"]]
        baseMeanCol <- metadata(data)[["baseMeanCol"]]
        isDegCol <- metadata(data)[["isDegCol"]]
        lfcCol <- metadata(data)[["lfcCol"]]
        assert(
            isString(alphaCol),
            isString(baseMeanCol),
            isString(isDegCol),
            isString(lfcCol)
        )
        keep <- complete.cases(data[, c(alphaCol, lfcCol)])
        data <- data[keep, , drop = FALSE]
        ## Define the limits and correct outliers, if necessary.
        if (is.null(limits[["x"]])) {
            limits[["x"]] <- c(
                min(floor(data[[lfcCol]])),
                max(ceiling(data[[lfcCol]]))
            )
        }
        assert(hasLength(limits[["x"]], n = 2L))
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
            isInRange(limits[["y"]][[2L]], lower = 1e-1, upper = 1L)
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
            data = as.data.frame(data),
            mapping = aes(
                x = !!sym(lfcCol),
                y = !!sym(negLogAlphaCol),
                color = !!sym(isDegCol)
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
            guides(color = "none")
        ## Labels.
        labels[["x"]] <- "log2 fold change"
        labels[["y"]] <- paste(
            "-log10",
            switch(
                EXPR = alphaCol,
                "padj" = "adjusted p",
                "svalue" = "s"
            ),
            "value"
        )
        if (isTRUE(labels[["title"]])) {
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
                baseMeanThreshold = baseMeanThreshold,
                lfcShrinkType = lfcShrinkType,
                lfcThreshold = lfcThreshold
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
        if (isTRUE(isPositive(ntop))) {
            assert(hasRownames(data))
            idx <- head(which(!is.na(data[["rank"]])), n = ntop)
            genes <- rownames(data)[idx]
        }
        ## Visualize specific genes on the plot, if desired.
        if (isCharacter(genes)) {
            assert(isSubset(genes, rownames(object)))
            diff <- setdiff(genes, rownames(data))
            genes <- intersect(genes, rownames(data))
            assert(hasLength(genes))
            alertInfo(sprintf(
                "Labeling %d %s in plot.",
                length(genes),
                ngettext(
                    n = length(genes),
                    msg1 = "gene",
                    msg2 = "genes"
                )
            ))
            if (hasLength(diff)) {
                alertWarning(sprintf(
                    paste(
                        "%d %s not labeled on plot,",
                        "due to censored adjusted P value: %s."
                    ),
                    length(diff),
                    ngettext(
                        n = length(diff),
                        msg1 = "gene",
                        msg2 = "genes"
                    ),
                    toInlineString(diff)
                ))
            }
            labelData <- data[unname(genes), , drop = FALSE]
            labelData[["geneName"]] <- rownames(labelData)
            p <- p +
                acid_geom_label_repel(
                    data = as.data.frame(labelData),
                    mapping = aes(
                        x = !!sym(lfcCol),
                        y = !!sym(negLogAlphaCol),
                        label = !!sym("geneName")
                    )
                )
        }
        ## Return --------------------------------------------------------------
        if (isTRUE(histograms)) {
            ## LFC density plot.
            lfcHist <- ggplot(
                data = as.data.frame(data),
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
                guides(fill = "none") +
                theme(
                    axis.line.y = element_blank(),
                    axis.text.y = element_blank(),
                    axis.ticks.y = element_blank()
                )
            ## P value density plot.
            pvalueHist <- ggplot(
                data = as.data.frame(data),
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
                guides(fill = "none") +
                theme(
                    axis.line.y = element_blank(),
                    axis.text.y = element_blank(),
                    axis.ticks.y = element_blank()
                )
            wrap_plots(
                list(p, lfcHist, pvalueHist),
                ## Alternatively can use `area()` here.
                design = paste("AA", "BC", sep = "\n"),
                heights = c(0.75, 0.25)
            )
        } else {
            p
        }
    }



#' @describeIn plotVolcano Passes to `DESeqResults` method, with `gene2symbol`
#' argument automatically defined.
#' @export
setMethod(
    f = "plotVolcano",
    signature = signature(object = "DESeqAnalysis"),
    definition = `plotVolcano,DESeqAnalysis`
)

#' @rdname plotVolcano
#' @export
setMethod(
    f = "plotVolcano",
    signature = signature(object = "DESeqResults"),
    definition = `plotVolcano,DESeqResults`
)
