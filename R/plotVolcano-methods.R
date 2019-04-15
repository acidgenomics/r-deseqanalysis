#' @name plotVolcano
#' @author Michael Steinbaugh, John Hutchinson, Lorena Pantano
#' @inherit bioverbs::plotVolcano
#' @inheritParams basejump::params
#' @inheritParams params
#'
#' @param ylim `numeric(1)`.
#'   Upper boundary limit for y-axis. Helps preserve dynamic range for gene sets
#'   containing highly significant P values (e.g. `1e-100`).
#' @param histograms `logical(1)`.
#'   Show LFC and P value histograms.
#'
#' @seealso Modification of `CHBUtils::volcano_density_plot()`.
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
#' plotVolcano(deseq, results = 1L)
#'
#' ## Customize the colors.
#' plotVolcano(
#'     object = deseq,
#'     results = 1L,
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
#'     results = 1L,
#'     direction = "up",
#'     ntop = 5L
#' )
#' plotVolcano(
#'     object = deseq,
#'     results = 1L,
#'     direction = "down",
#'     ntop = 5L
#' )
#'
#' ## Label genes manually.
#' ## Note that either gene IDs or names (symbols) are supported.
#' plotVolcano(deseq, results = 1L, genes = genes)
NULL



#' @rdname plotVolcano
#' @name plotVolcano
#' @importFrom bioverbs plotVolcano
#' @export
NULL



plotVolcano.DESeqResults <-  # nolint
    function(
        object,
        ylim = 1e-10,
        genes = NULL,
        gene2symbol = NULL,
        ntop = 0L,
        direction = c("both", "up", "down"),
        pointColor = c(
            downregulated = "orange",
            nonsignificant = "gray50",
            upregulated = "purple"
        ),
        pointSize = 2L,
        pointAlpha = 0.7,
        histograms = FALSE,
        return = c("ggplot", "DataFrame")
    ) {
        validObject(object)
        alpha <- metadata(object)[["alpha"]]
        lfcThreshold <- metadata(object)[["lfcThreshold"]]
        assert(
            isAlpha(alpha),
            isNumber(lfcThreshold),
            isNonNegative(lfcThreshold),
            isNumber(ylim),
            isInRange(ylim, lower = 1e-100, upper = 1e-3),
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
            isFlag(histograms)
        )
        direction <- match.arg(direction)
        return <- match.arg(return)

        # Check to see if we should use `sval` instead of `padj`
        if ("svalue" %in% names(object)) {
            testCol <- "svalue"  # nocov
        } else {
            testCol <- "padj"
        }

        # Placeholder variable for matching the LFC column.
        lfcCol <- "log2FoldChange"
        negLogTestCol <- camel(paste("neg", "log10", testCol))

        data <- object %>%
            as_tibble(rownames = "rowname") %>%
            camel() %>%
            # Select columns used for plots.
            select(!!!syms(c("rowname", "baseMean", lfcCol, testCol))) %>%
            # Remove genes with NA adjusted P values.
            filter(!is.na(!!sym(testCol))) %>%
            # Remove genes with zero counts.
            filter(!!sym("baseMean") > 0L) %>%
            # Negative log10 transform the test values. Add `ylim` here to
            # prevent `Inf` values resulting from log transformation.
            # This will also define the upper bound of the y-axis.
            # Then calculate the rank score, which is used for `ntop`.
            mutate(
                !!sym(negLogTestCol) := -log10(!!sym(testCol) + !!ylim),
                rankScore = !!sym(negLogTestCol) * abs(!!sym(lfcCol))
            ) %>%
            arrange(desc(!!sym("rankScore"))) %>%
            mutate(rank = row_number()) %>%
            .addIsDECol(
                testCol = testCol,
                alpha = alpha,
                lfcThreshold = lfcThreshold
            )

        # Apply directional filtering, if desired.
        if (direction == "up") {
            data <- filter(data, !!sym(lfcCol) > 0L)
        } else if (direction == "down") {
            data <- filter(data, !!sym(lfcCol) < 0L)
        }

        # Early return the data, if desired.
        if (return == "DataFrame") {
            return(as(data, "DataFrame"))
        }

        # LFC density ----------------------------------------------------------
        lfcHist <- ggplot(
            data = data,
            mapping = aes(x = !!sym(lfcCol))
        ) +
            geom_density(
                colour = NA,
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

        # P value density ------------------------------------------------------
        pvalueHist <- ggplot(
            data = data,
            mapping = aes(x = !!sym(negLogTestCol))
        ) +
            geom_density(
                colour = NA,
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

        # Volcano plot ---------------------------------------------------------
        p <- ggplot(
            data = data,
            mapping = aes(
                x = !!sym(lfcCol),
                y = !!sym(negLogTestCol),
                colour = !!sym("isDE")
            )
        ) +
            geom_vline(
                xintercept = 0L,
                size = 0.5,
                colour = pointColor[["nonsignificant"]]
            ) +
            geom_point(
                alpha = pointAlpha,
                size = pointSize,
                stroke = 0L
            ) +
            scale_x_continuous(breaks = pretty_breaks()) +
            scale_y_continuous(breaks = pretty_breaks()) +
            guides(colour = FALSE) +
            labs(
                title = contrastName(object),
                subtitle = paste("alpha", "<", alpha),
                x = "log2 fold change",
                y = "-log10 adj p value"
            )

        if (isCharacter(pointColor)) {
            p <- p +
                scale_colour_manual(
                    values = c(
                        "-1" = pointColor[["downregulated"]],
                        "0" = pointColor[["nonsignificant"]],
                        "1" = pointColor[["upregulated"]]
                    )
                )
        }

        # Gene text labels -----------------------------------------------------
        # Get the genes to visualize when `ntop` is declared.
        if (ntop > 0L) {
            assert(
                isSubset(c("rowname", "rank"), colnames(data)),
                # Double check that data is arranged by `rank` column.
                identical(data[["rank"]], sort(data[["rank"]]))
            )
            # Since we know the data is arranged by rank, simply take the head.
            genes <- head(data[["rowname"]], n = ntop)
        }

        # Visualize specific genes on the plot, if desired.
        if (!is.null(genes)) {
            validObject(gene2symbol)
            assert(matchesGene2Symbol(
                x = object,
                genes = genes,
                gene2symbol = gene2symbol
            ))
            # Map the user-defined `genes` to `gene2symbol` rownames.
            # We're using this to match back to the `DESeqResults` object.
            rownames <- mapGenesToRownames(
                object = gene2symbol,
                genes = genes
            )
            # Prepare the label data tibble.
            labelData <- data %>%
                .[match(x = rownames, table = .[["rowname"]]), ] %>%
                left_join(as(gene2symbol, "tbl_df"), by = "rowname")
            p <- p +
                acid_geom_label_repel(
                    data = labelData,
                    mapping = aes(
                        x = !!sym(lfcCol),
                        y = !!sym(negLogTestCol),
                        label = !!sym("geneName")
                    )
                )
        }

        # Return ---------------------------------------------------------------
        if (isTRUE(histograms)) {
            ggdraw() +
                # Coordinates are relative to lower left corner
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



#' @rdname plotVolcano
#' @export
setMethod(
    f = "plotVolcano",
    signature = signature("DESeqResults"),
    definition = plotVolcano.DESeqResults
)



plotVolcano.DESeqAnalysis <-  # nolint
    function(
        object,
        results,
        lfcShrink = TRUE
    ) {
        validObject(object)
        assert(
            isScalar(results),
            isFlag(lfcShrink)
        )
        # Return `NULL` for objects that don't contain gene symbol mappings.
        gene2symbol <- tryCatch(
            expr = Gene2Symbol(slot(object, "data")),
            error = function(e) NULL
        )
        do.call(
            what = plotVolcano.DESeqResults,
            args = matchArgsToDoCall(
                args = list(
                    object = .matchResults(
                        object = object,
                        results = results,
                        lfcShrink = lfcShrink
                    ),
                    genes = genes,
                    gene2symbol = gene2symbol
                ),
                removeFormals = c("results", "lfcShrink")
            )
        )
    }

f1 <- formals(plotVolcano.DESeqAnalysis)
f2 <- formals(plotVolcano.DESeqResults)
f2 <- f2[setdiff(names(f2), c(names(f1), "gene2symbol"))]
f <- c(f1, f2)
formals(plotVolcano.DESeqAnalysis) <- f



#' @rdname plotVolcano
#' @export
setMethod(
    f = "plotVolcano",
    signature = signature("DESeqAnalysis"),
    definition = plotVolcano.DESeqAnalysis
)
