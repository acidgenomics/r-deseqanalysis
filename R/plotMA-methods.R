#' @name plotMA
#' @author Michael Steinbaugh, Rory Kirchner
#' @inherit BiocGenerics::plotMA
#' @note We are not allowing post hoc `alpha` or `lfcThreshold` cutoffs here.
#' @note Updated 2019-11-19.
#'
#' @details
#' An MA plot is an application of a Bland–Altman plot for visual representation
#' of genomic data. The plot visualizes the differences between measurements
#' taken in two samples, by transforming the data onto M (log ratio) and A
#' (mean average) scales, then plotting these values.
#'
#' @section plotMA2 aliases:
#'
#' Aliased methods for original [DESeq2::plotMA()] S4 methods, which us
#' geneplotter instead of ggplot2. I prefer using ggplot2 instead, so the
#' primary methods defined here in the package mask DESeq2.
#'
#' @inheritParams DESeq2::plotMA
#' @inheritParams acidroxygen::params
#' @inheritParams params
#' @param ... Additional arguments.
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



#' @rdname plotMA
#' @name plotMA
#' @importFrom BiocGenerics plotMA
#' @usage plotMA(object, ...)
#' @export
NULL



## Updated 2019-09-13.
`plotMA,DESeqResults` <-  # nolint
    function(
        object,
        genes = NULL,
        gene2symbol = NULL,
        ntop = 0L,
        direction = c("both", "up", "down"),
        pointColor = c(
            downregulated = acidplots::lightPalette[["purple"]],
            upregulated = acidplots::lightPalette[["orange"]],
            nonsignificant = acidplots::lightPalette[["gray"]]
        ),
        pointSize = 2L,
        pointAlpha = 0.8,
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
        ## Genes or ntop, but not both.
        if (!is.null(genes) && ntop > 0L) {
            stop("Specify either 'genes' or 'ntop'.")
        }
        ## Check to see if we should use `sval` column instead of `padj`.
        if ("svalue" %in% names(object)) {
            testCol <- "svalue"  # nocov
        } else {
            testCol <- "padj"
        }
        ## Placeholder variables.
        lfcCol <- "log2FoldChange"
        ## Note that `lfcShrink()` doesn't return `stat` column.
        if ("stat" %in% names(object)) {
            rankCol <- "stat"
        } else {
            rankCol <- lfcCol
        }
        data <- as(object, "DataFrame")
        data <- camelCase(data)
        assert(isSubset(
            x = c("baseMean", lfcCol, rankCol, testCol),
            y = colnames(data)
        ))
        ## Remove genes with very low expression.
        keep <- which(data[["baseMean"]] >= 1L)
        data <- data[keep, , drop = FALSE]
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
            lfcCol = lfcCol,
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
        ## Check for no genes passing cutoffs and early return.
        if (!hasRows(data)) {
            message("No genes passed cutoffs.")
            return(invisible())
        }
        ## Early return the data, if desired.
        if (identical(return, "DataFrame")) {
            return(data)
        }

        ## MA plot -------------------------------------------------------------
        log10BaseMean <- log10(data[["baseMean"]])
        floor <- min(floor(log10BaseMean))
        ceiling <- max(ceiling(log10BaseMean))
        xBreaks <- 10L ^ seq(from = floor, to = ceiling, by = 1L)
        p <- ggplot(
            data = as_tibble(data, rownames = NULL),
            mapping = aes(
                x = !!sym("baseMean"),
                y = !!sym(lfcCol),
                color = !!sym("isDE")
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
                limits = c(1L, NA),
                trans = "log10"
            ) +
            scale_y_continuous(breaks = pretty_breaks()) +
            annotation_logticks(sides = "b") +
            guides(color = FALSE) +
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
        p
    }



#' @rdname plotMA
#' @export
setMethod(
    f = "plotMA",
    signature = signature("DESeqResults"),
    definition = `plotMA,DESeqResults`
)



## Updated 2019-11-19.
`plotMA,DESeqAnalysis` <-  # nolint
    function(
        object,
        i,
        lfcShrink = TRUE,
        ...
    ) {
        ## nocov start
        call <- match.call()
        ## results
        if ("results" %in% names(call)) {
            stop("'results' is defunct in favor of 'i'.")
        }
        rm(call)
        ## nocov end
        validObject(object)
        assert(
            isScalar(i),
            isFlag(lfcShrink)
        )
        ## Return `NULL` for objects that don't contain gene symbol mappings.
        gene2symbol <- tryCatch(
            expr = suppressMessages(
                Gene2Symbol(as(object, "DESeqDataSet"))
            ),
            error = function(e) NULL
        )
        ## Handoff to DESeqResults method.
        plotMA(
            object = results(
                object = object,
                i = i,
                lfcShrink = lfcShrink
            ),
            gene2symbol = gene2symbol,
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
