#' MA plot
#'
#' @name plotMa
#' @author Michael Steinbaugh, Rory Kirchner
#' @note Updated 2023-12-18.
#'
#' @description The plot visualizes the differences between measurements taken
#' in two samples, by transforming the data onto **M** (log ratio) and **A**
#' (mean average) scales.
#'
#' @details
#' An MA plot is an application of a Bland–Altman plot for visual
#' representation of genomic data. The plot visualizes the differences between
#' measurements taken in two samples, by transforming the data onto
#' M (log ratio) and A (mean average) scales, then plotting these values.
#'
#' @inheritParams AcidRoxygen::params
#' @inheritParams params
#' @param ... Additional arguments.
#'
#' @seealso `DESeq2::plotMA()`.
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
#' plotMa(deseq, i = 1L)
#'
#' ## Customize the colors.
#' plotMa(
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
#' plotMa(deseq, i = 1L, direction = "up", ntop = 5L)
#' plotMa(deseq, i = 1L, direction = "down", ntop = 5L)
#'
#' ## Label genes manually.
#' ## Note that either gene IDs or names (symbols) are supported.
#' plotMa(deseq, i = 1L, genes = genes)
NULL



## Updated 2023-12-18.
`plotMa,DESeqAnalysis` <- # nolint
    function(object,
             i,
             alphaThreshold = NULL,
             baseMeanThreshold = NULL,
             lfcThreshold = NULL,
             genes = NULL,
             ntop = 0L,
             ...) {
        assert(
            validObject(object),
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
            rownames(res) <- rownames(dds)
        }
        plotMa(
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



## Updated 2023-12-18.
`plotMa,DESeqResults` <- # nolint
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
             limits = list("x" = NULL, "y" = NULL),
             labels = list(
                 "title" = TRUE,
                 "subtitle" = NULL
             )) {
        if (is.null(alphaThreshold)) {
            alphaThreshold <- alphaThreshold(object)
        }
        if (is.null(baseMeanThreshold)) {
            baseMeanThreshold <- baseMeanThreshold(object)
        }
        ## We're applying log10 transformation on plot, so gate the minimum.
        if (isTRUE(baseMeanThreshold < 1L)) {
            baseMeanThreshold <- 1L
        }
        if (is.null(lfcThreshold)) {
            lfcThreshold <- lfcThreshold(object)
        }
        lfcShrinkType <- lfcShrinkType(object)
        assert(
            isAlpha(alphaThreshold),
            isNumber(baseMeanThreshold),
            isPositive(baseMeanThreshold),
            isNumber(lfcThreshold),
            isNonNegative(lfcThreshold),
            isString(lfcShrinkType),
            isAny(genes, classes = c("character", "NULL")),
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
            areSetEqual(names(limits), c("x", "y"))
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
        baseMeanCol <- metadata(data)[["baseMeanCol"]]
        isDegCol <- metadata(data)[["isDegCol"]]
        lfcCol <- metadata(data)[["lfcCol"]]
        assert(
            isString(baseMeanCol),
            isString(isDegCol),
            isString(lfcCol)
        )
        ## Define the limits and correct outliers, if necessary.
        if (is.null(limits[["x"]])) {
            limits[["x"]] <- c(
                min(floor(data[[baseMeanCol]])),
                max(ceiling(data[[baseMeanCol]]))
            )
        }
        assert(
            hasLength(limits[["x"]], n = 2L),
            allArePositive(limits[["x"]])
        )
        ok <- list(
            data[[baseMeanCol]] >= limits[["x"]][[1L]],
            data[[baseMeanCol]] <= limits[["x"]][[2L]]
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
            data[[baseMeanCol]][!ok[[1L]]] <- limits[["x"]][[1L]]
            data[[baseMeanCol]][!ok[[2L]]] <- limits[["x"]][[2L]]
        }
        if (is.null(limits[["y"]])) {
            limits[["y"]] <- c(
                min(floor(data[[lfcCol]])),
                max(ceiling(data[[lfcCol]]))
            )
        }
        assert(hasLength(limits[["y"]], n = 2L))
        ok <- list(
            data[[lfcCol]] >= limits[["y"]][[1L]],
            data[[lfcCol]] <= limits[["y"]][[2L]]
        )
        if (!all(unlist(ok))) {
            n <- sum(!unlist(ok))
            alertWarning(sprintf(
                "%d %s outside y-axis limits of {.var c(%s, %s)}.",
                n,
                ngettext(n = n, msg1 = "point", msg2 = "points"),
                limits[["y"]][[1L]],
                limits[["y"]][[2L]]
            ))
            data[[lfcCol]][!ok[[1L]]] <- limits[["y"]][[1L]]
            data[[lfcCol]][!ok[[2L]]] <- limits[["y"]][[2L]]
        }
        breaks <- list(
            "x" = 10L^seq(
                from = min(floor(log10(limits[["x"]][[1L]]))),
                to = min(floor(log10(limits[["x"]][[2L]]))),
                by = 1L
            ),
            "y" = pretty_breaks()
        )
        p <- ggplot(
            data = as.data.frame(data),
            mapping = aes(
                x = .data[[baseMeanCol]],
                y = .data[[lfcCol]],
                color = .data[[isDegCol]]
            )
        ) +
            geom_hline(
                color = pointColor[["nonsignificant"]],
                linewidth = 0.5,
                yintercept = 0L
            ) +
            geom_point(
                alpha = pointAlpha,
                size = pointSize,
                stroke = 0L
            ) +
            scale_x_continuous(
                breaks = breaks[["x"]],
                limits = limits[["x"]],
                trans = "log10"
            ) +
            scale_y_continuous(
                breaks = breaks[["y"]],
                limits = limits[["y"]],
                trans = "identity"
            ) +
            annotation_logticks(sides = "b") +
            guides(color = "none")
        ## Labels.
        labels[["x"]] <- "mean expression across all samples"
        labels[["y"]] <- "log2 fold change"
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
        ## Color the significant points.
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
        if (isTRUE(isPositive(ntop))) {
            assert(hasRownames(data))
            idx <- head(which(!is.na(data[["rank"]])), n = ntop)
            genes <- rownames(data)[idx]
        }
        ## Visualize specific genes on the plot, if desired.
        if (isCharacter(genes)) {
            assert(isSubset(genes, rownames(object)))
            alertInfo(sprintf(
                "Labeling %d %s in plot.",
                length(genes),
                ngettext(
                    n = length(genes),
                    msg1 = "gene",
                    msg2 = "genes"
                )
            ))
            labelData <- data[genes, , drop = FALSE]
            labelData[["geneName"]] <- rownames(labelData)
            p <- p +
                acid_geom_label_repel(
                    data = as.data.frame(labelData),
                    mapping = aes(
                        x = .data[[baseMeanCol]],
                        y = .data[[lfcCol]],
                        label = .data[["geneName"]]
                    )
                )
        }
        p
    }



#' @describeIn plotMa Passes to `DESeqResults` method, with `gene2symbol`
#' argument automatically defined.
#' @export
setMethod(
    f = "plotMa",
    signature = signature(object = "DESeqAnalysis"),
    definition = `plotMa,DESeqAnalysis`
)

#' @rdname plotMa
#' @export
setMethod(
    f = "plotMa",
    signature = signature(object = "DESeqResults"),
    definition = `plotMa,DESeqResults`
)
