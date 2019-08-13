#' @name plotMA
#' @author Michael Steinbaugh, Rory Kirchner
#' @inherit BiocGenerics::plotMA
#'
#' @inheritParams DESeq2::plotMA
#' @inheritParams acidroxygen::params
#' @inheritParams params
#' @param ... Additional arguments.
#'
#' @details
#' An MA plot is an application of a Bland–Altman plot for visual representation
#' of genomic data. The plot visualizes the differences between measurements
#' taken in two samples, by transforming the data onto M (log ratio) and A
#' (mean average) scales, then plotting these values.
#'
#' @note We are not allowing post hoc `alpha` or `lfcThreshold` cutoffs here.
#'
#' @section plotMA2 aliases:
#'
#' Aliased methods for original [DESeq2::plotMA()] S4 methods, which us
#' geneplotter instead of ggplot2. I prefer using ggplot2 instead, so the
#' primary methods defined here in the package mask DESeq2.
#'
#' @return `ggplot`.
#'
#' @seealso [DESeq2::plotMA()].
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
#' plotMA(deseq, results = 1L)
#'
#' ## Customize the colors.
#' plotMA(
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
#' plotMA(deseq, results = 1L, direction = "up", ntop = 5L)
#' plotMA(deseq, results = 1L, direction = "down", ntop = 5L)
#'
#' ## Label genes manually.
#' ## Note that either gene IDs or names (symbols) are supported.
#' plotMA(deseq, results = 1L, genes = genes)
NULL



#' @rdname plotMA
#' @name plotMA
#' @importFrom BiocGenerics plotMA
#' @usage plotMA(object, ...)
#' @export
NULL



## Updated 2019-07-23.
`plotMA,DESeqResults` <-  # nolint
    function(
        object,
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
        return <- match.arg(return)

        if (!is.null(genes) && ntop > 0L) {
            stop("Specify either 'genes' or 'ntop'.")
        }

        ## Check to see if we should use `sval` column instead of `padj`.
        if ("svalue" %in% names(object)) {
            testCol <- "svalue"  # nocov
        } else {
            testCol <- "padj"
        }

        ## Placeholder variable for matching the LFC column.
        lfcCol <- "log2FoldChange"

        data <- object %>%
            as_tibble(rownames = "rowname") %>%
            camelCase() %>%
            ## Remove genes with very low expression.
            filter(!!sym("baseMean") >= 1L) %>%
            mutate(rankScore = abs(!!sym("log2FoldChange"))) %>%
            arrange(desc(!!sym("rankScore"))) %>%
            mutate(rank = row_number()) %>%
            .addIsDECol(
                testCol = testCol,
                alpha = alpha,
                lfcCol = lfcCol,
                lfcThreshold = lfcThreshold
            )
        assert(isSubset(
            x = c(
                "rowname",
                "baseMean",
                lfcCol,
                testCol,
                "rankScore",
                "rank",
                "isDE"
            ),
            y = colnames(data)
        ))

        ## Apply directional filtering, if desired.
        if (direction == "up") {
            data <- filter(data, !!sym(lfcCol) > 0L)
        } else if (direction == "down") {
            data <- filter(data, !!sym(lfcCol) < 0L)
        }

        ## Check for no genes passing cutoffs and early return.
        if (!nrow(data)) {
            warning("No genes passed cutoffs.")
            return(invisible())
        }

        ## Early return the data, if desired.
        if (return == "DataFrame") {
            return(as(data, "DataFrame"))
        }

        ## MA plot -------------------------------------------------------------
        log10BaseMean <- log10(data[["baseMean"]])
        floor <- min(floor(log10BaseMean))
        ceiling <- max(ceiling(log10BaseMean))
        xBreaks <- 10L ^ seq(from = floor, to = ceiling, by = 1L)

        p <- ggplot(
            data = data,
            mapping = aes(
                x = !!sym("baseMean"),
                y = !!sym(lfcCol),
                colour = !!sym("isDE")
            )
        ) +
            geom_hline(
                yintercept = 0L,
                size = 0.5,
                colour = pointColor[["nonsignificant"]]
            ) +
            geom_point(
                alpha = pointAlpha,
                size = pointSize,
                stroke = 0L
            ) +
            scale_x_continuous(
                breaks = xBreaks,
                limits = c(1L, NA),
                trans = "log10"
            ) +
            scale_y_continuous(breaks = pretty_breaks()) +
            annotation_logticks(sides = "b") +
            guides(colour = FALSE) +
            labs(
                title = contrastName(object),
                subtitle = paste0(
                    "alpha: ", alpha, ";  ",
                    "lfcThreshold: ", lfcThreshold, ";  ",
                    "lfcShrink: ", lfcShrinkType
                ),
                x = "mean expression across all samples",
                y = "log2 fold change"
            )

        ## Color the significant points.
        ## Note that we're using direction-specific coloring by default.
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
        if (ntop > 0L) {
            assert(
                isSubset(c("rowname", "rank"), colnames(data)),
                ## Double check that data is arranged by `rank` column.
                identical(data[["rank"]], sort(data[["rank"]]))
            )
            ## Since we know the data is arranged by rank, simply take the head.
            genes <- head(data[["rowname"]], n = ntop)
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
            rownames <- mapGenesToRownames(
                object = gene2symbol,
                genes = genes
            )

            ## Prepare the label data tibble.
            labelData <- data %>%
                .[match(x = rownames, table = .[["rowname"]]), ] %>%
                left_join(as(gene2symbol, "tbl_df"), by = "rowname")

            p <- p +
                acid_geom_label_repel(
                    data = labelData,
                    mapping = aes(
                        x = !!sym("baseMean"),
                        y = !!sym(lfcCol),
                        label = !!sym("geneName")
                    )
                )
        }

        ## Return --------------------------------------------------------------
        p
    }



#' @rdname plotMA
#' @export
setMethod(
    f = "plotMA",
    signature = signature("DESeqResults"),
    definition = `plotMA,DESeqResults`
)



## Updated 2019-07-23.
`plotMA,DESeqAnalysis` <-  # nolint
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
        results <- results(object, results = results, lfcShrink = lfcShrink)
        do.call(
            what = plotMA,
            args = matchArgsToDoCall(
                args = list(
                    object = results,
                    genes = genes,
                    gene2symbol = gene2symbol
                ),
                removeFormals = c("results", "lfcShrink")
            )
        )
    }

f1 <- formals(`plotMA,DESeqAnalysis`)
f2 <- formals(`plotMA,DESeqResults`)
f2 <- f2[setdiff(names(f2), c(names(f1), "gene2symbol"))]
f <- c(f1, f2)
formals(`plotMA,DESeqAnalysis`) <- f



#' @rdname plotMA
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
    plotMA(...)
}
