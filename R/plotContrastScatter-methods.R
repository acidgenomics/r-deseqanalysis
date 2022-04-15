#' @name plotContrastScatter
#' @inherit AcidGenerics::plotContrastScatter
#' @note Updated 2022-04-15.
#'
#' @inheritParams AcidRoxygen::params
#' @inheritParams params
#' @param ... Additional arguments.
#'
#' @seealso
#' - https://doi.org/10.1084/jem.20200829
#'
#' @examples
#' data(deseq)
#'
#' ## DESeqAnalysis ====
#' plotContrastScatter(deseq, i = 1L)
NULL



## Updated 2022-04-15.
`plotContrastScatter,DESeqAnalysis` <-
    function(object,
             i,
             direction = c("both", "up", "down"),
             alphaThreshold = NULL,
             baseMeanThreshold = NULL,
             lfcThreshold = NULL,
             pointColor = c(
                 "downregulated" = AcidPlots::lightPalette[["purple"]],
                 "upregulated" = AcidPlots::lightPalette[["orange"]],
                 "nonsignificant" = AcidPlots::lightPalette[["gray"]]
             ),
             pointSize = 2L,
             pointAlpha = 0.8,
             trans = c("log2", "log10", "identity"),
             limits = list("x" = NULL, "y" = NULL),
             labels = list(
                 "title" = TRUE,
                 "subtitle" = NULL,
                 "x" = TRUE,
                 "y" = TRUE
             )) {
        validObject(object)
        direction <- match.arg(direction)
        trans <- match.arg(trans)
        if (is.null(alphaThreshold)) {
            alphaThreshold <- alphaThreshold(object)
        }
        if (is.null(baseMeanThreshold)) {
            baseMeanThreshold <- baseMeanThreshold(object)
        }
        if (!identical(trans, "identity") && isTRUE(baseMeanThreshold < 1L)) {
            baseMeanThreshold <- 1L
        }
        if (is.null(lfcThreshold)) {
            lfcThreshold <- lfcThreshold(object)
        }
        assert(
            isAlpha(alphaThreshold),
            isNumber(baseMeanThreshold),
            isPositive(baseMeanThreshold),
            isNumber(lfcThreshold),
            isNonNegative(lfcThreshold),
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
        labels <- matchLabels(labels)
        contrastMeta <- contrastSamples(
            object = object,
            i = i,
            quiet = FALSE,
            return = "list"
        )
        assert(
            is.list(contrastMeta),
            identical(
                x = names(contrastMeta),
                y = c("contrast", "samples")
            ),
            identical(
                x = names(contrastMeta[["samples"]]),
                y = c("numerator", "denominator")
            )
        )
        dds <- as(object, "DESeqDataSet")
        res <- results(object, i = i, extra = FALSE)
        assert(identical(rownames(dds), rownames(res)))
        resDf <- .prepareResultsForPlot(
            object = res,
            direction = direction,
            alphaThreshold = alphaThreshold,
            baseMeanThreshold = baseMeanThreshold,
            lfcThreshold = lfcThreshold
        )
        if (!hasRows(resDf)) {
            return(invisible(NULL))
        }
        assert(isSubset("isDegCol", names(metadata(resDf))))
        isDegCol <- metadata(resDf)[["isDegCol"]]
        assert(isString(isDegCol))
        dds <- dds[
            rownames(resDf),
            c(
                contrastMeta[["samples"]][["numerator"]],
                contrastMeta[["samples"]][["denominator"]]
            ),
            drop = FALSE
        ]
        counts <- counts(dds, normalized = TRUE)
        ## Only include non-zero counts on the plot.
        keep <- rowSums(counts) > 0L
        counts <- counts[keep, , drop = FALSE]
        if (!hasRows(counts)) {
            return(invisible(NULL))
        }
        ## Apply log2 or log10 transformation, when applicable.
        if (!identical(trans, "identity")) {
            fun <- get(trans, inherits = TRUE)
            assert(is.function(fun))
            counts <- fun(counts + 1L)
        }
        resDf <- resDf[rownames(counts), , drop = FALSE]
        data <- data.frame(
            "x" = rowMeans(
                counts[, contrastMeta[["samples"]][["denominator"]]]
            ),
            "y" = rowMeans(
                counts[, contrastMeta[["samples"]][["numerator"]]]
            ),
            "isDeg" = resDf[[isDegCol]],
            row.names = rownames(counts)
        )
        p <- ggplot(
            data = data,
            mapping = aes(
                x = !!sym("x"),
                y = !!sym("y")
            )
        ) +
            geom_point(
                mapping = aes(color = !!sym(isDegCol)),
                alpha = pointAlpha,
                size = pointSize,
                stroke = 0L,
                show.legend = FALSE
            )
        ## Labels.
        if (isTRUE(labels[["x"]])) {
            labels[["x"]] <- toString(
                x = contrastMeta[["samples"]][["denominator"]],
                width = 100L
            )
        }
        if (isTRUE(labels[["y"]])) {
            labels[["y"]] <- toString(
                x = contrastMeta[["samples"]][["numerator"]],
                width = 100L
            )
        }
        if (isTRUE(labels[["title"]])) {
            labels[["title"]] <- tryCatch(
                expr = contrastName(res),
                error = function(e) NULL
            )
        }
        if (is.null(labels[["subtitle"]])) {
            labels[["subtitle"]] <- .thresholdLabel(
                object = object,
                direction = direction,
                alphaThreshold = alphaThreshold,
                baseMeanThreshold = baseMeanThreshold,
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
        p
    }



#' @rdname plotContrastScatter
#' @export
setMethod(
    f = "plotContrastScatter",
    signature = signature(object = "DESeqAnalysis"),
    definition = `plotContrastScatter,DESeqAnalysis`
)
