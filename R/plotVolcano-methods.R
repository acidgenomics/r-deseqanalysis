#' @name plotVolcano
#' @author Michael Steinbaugh, John Hutchinson, Lorena Pantano
#' @inherit bioverbs::plotVolcano
#' @note Updated 2019-09-10.
#'
#' @inheritParams acidroxygen::params
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
#' @usage plotVolcano(object, ...)
#' @export
NULL



## Updated 2019-08-27.
`plotVolcano,DESeqResults` <-  # nolint
    function(
        object,
        ylim = 1e-10,
        genes = NULL,
        gene2symbol = NULL,
        ntop = 0L,
        direction = c("both", "up", "down"),
        pointColor = c(
            downregulated = "darkorchid3",
            upregulated = "darkorange2",
            nonsignificant = "gray50"
        ),
        pointSize = 2L,
        pointAlpha = 0.7,
        histograms = FALSE,
        return = c("ggplot", "DataFrame")
    ) {
        validObject(object)
        alpha <- metadata(object)[["alpha"]]
        lfcThreshold <- metadata(object)[["lfcThreshold"]]
        lfcShrinkType <- lfcShrinkType(object)
        assert(
            isAlpha(alpha),
            isNumber(lfcThreshold),
            isNonNegative(lfcThreshold),
            isString(lfcShrinkType),
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
        ## Check to see if we should use `sval` instead of `padj`
        if ("svalue" %in% names(object)) {
            testCol <- "svalue"  # nocov
        } else {
            testCol <- "padj"
        }
        ## Placeholder variables.
        lfcCol <- "log2FoldChange"
        negLogTestCol <- camelCase(paste("neg", "log10", testCol))
        ## Note that `lfcShrink()` doesn't return `stat` column.
        if ("stat" %in% names(object)) {
            rankCol <- "stat"
        } else {
            rankCol <- negLogTestCol
        }
        data <- as(object, "DataFrame")
        data <- camelCase(data)
        assert(isSubset(
            x = c("baseMean", lfcCol, testCol),
            y = colnames(data)
        ))
        ## Remove genes with NA adjusted P values.
        keep <- which(!is.na(data[[testCol]]))
        data <- data[keep, , drop = FALSE]
        ## Remove genes with zero counts.
        keep <- which(data[["baseMean"]] > 0L)
        data <- data[keep, , drop = FALSE]
        ## Negative log10 transform the test values. Add `ylim` here to prevent
        ## `Inf` values resulting from log transformation. This will also define
        ## the upper bound of the y-axis. Then calculate the rank score, which
        ## is used for `ntop`.
        data[[negLogTestCol]] <- -log10(data[[testCol]] + ylim)
        data[["rankScore"]] <- abs(data[[rankCol]])
        data <- data[
            order(data[["rankScore"]], decreasing = TRUE),
            ,
            drop = FALSE
            ]
        data[["rank"]] <- seq_len(nrow(data))
        data <- .addIsDECol(
            data = data,
            testCol = testCol,
            alpha = alpha,
            lfcThreshold = lfcThreshold
        )
        assert(isSubset(
            x = c("isDE", "rank", "rankScore"),
            y = colnames(data)
        ))
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

        ## P value density -----------------------------------------------------
        pvalueHist <- ggplot(
            data = as_tibble(data, rownames = NULL),
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

        ## Volcano plot --------------------------------------------------------
        p <- ggplot(
            data = as_tibble(data, rownames = NULL),
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
                subtitle = paste0(
                    "alpha: ", alpha, ";  ",
                    "lfcThreshold: ", lfcThreshold, ";  ",
                    "lfcShrink: ", lfcShrinkType
                ),
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



#' @rdname plotVolcano
#' @export
setMethod(
    f = "plotVolcano",
    signature = signature("DESeqResults"),
    definition = `plotVolcano,DESeqResults`
)



## Updated 2019-09-10.
`plotVolcano,DESeqAnalysis` <-  # nolint
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
        ## Return `NULL` for objects that don't contain gene symbol mappings.
        gene2symbol <- tryCatch(
            expr = suppressMessages(
                Gene2Symbol(as(object, "DESeqDataSet"))
            ),
            error = function(e) NULL
        )
        data <- results(object, results = results, lfcShrink = lfcShrink)
        if (!isString(results)) {
            name <- contrastNames(object)[[results]]
        } else {
            name <- results
        }
        contrastName(data) <- name
        do.call(
            what = `plotVolcano,DESeqResults`,
            args = matchArgsToDoCall(
                args = list(
                    object = data,
                    genes = genes,
                    gene2symbol = gene2symbol
                ),
                removeFormals = c("results", "lfcShrink")
            )
        )
    }

f1 <- formals(`plotVolcano,DESeqAnalysis`)
f2 <- formals(`plotVolcano,DESeqResults`)
f2 <- f2[setdiff(names(f2), c(names(f1), "gene2symbol"))]
f <- c(f1, f2)
formals(`plotVolcano,DESeqAnalysis`) <- f



#' @rdname plotVolcano
#' @export
setMethod(
    f = "plotVolcano",
    signature = signature("DESeqAnalysis"),
    definition = `plotVolcano,DESeqAnalysis`
)
