## FIXME This function is still a work in progress.
## FIXME Need to support plotVolcano colors here...
## FIXME Consider supporting directionality labeling.
## FIXME Consider plotting a regression line here?



#' @name plotContrastScatter
#' @inherit AcidGenerics::plotContrastScatter
#' @note Updated 2022-03-08.
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



## Updated 2021-10-15.
`plotContrastScatter,DESeqAnalysis` <-
    function(
        object,
        i,
        direction = c("both", "up", "down"),
        ## FIXME Need to support.
        alphaThreshold = NULL,
        ## FIXME Need to support.
        ## FIXME Need to filter against the DESeqResults to drop the number
        ## of genes first.
        baseMeanThreshold = NULL,
        ## FIXME Need to support.
        lfcThreshold = NULL,
        ## FIXME Need to support.
        genes = NULL,
        ## FIXME Need to support.
        ntop = 0L,
        ## FIXME Need to support.
        pointColor = c(
            "downregulated" = AcidPlots::lightPalette[["purple"]],
            "upregulated" = AcidPlots::lightPalette[["orange"]],
            "nonsignificant" = AcidPlots::lightPalette[["gray"]]
        ),
        ## FIXME Need to support.
        pointSize = 2L,
        ## FIXME Need to support.
        pointAlpha = 0.8,
        ## FIXME Need to support this.
        trans = c("log2", "log10", "identity"),
        ## FIXME Need to support.
        limits = list("x" = NULL, "y" = NULL),
        ## FIXME Need to support.
        ## FIXME Ensure that these work here...need to update.
        labels = list(
            "title" = TRUE,
            "subtitle" = NULL,
            "x" = TRUE,
            "y" = TRUE
        )
    ) {
        validObject(object)
        direction <- match.arg(direction)
        trans <- match.arg(trans)
        if (is.null(alphaThreshold)) {
            alphaThreshold <- alphaThreshold(object)
        }
        if (is.null(baseMeanThreshold)) {
            baseMeanThreshold <- baseMeanThreshold(object)
        }
        ## FIXME Only do this step when trans is not identity.
        ## We're applying log10 transformation on plot, so gate the minimum.
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
        labels <- matchLabels(labels)
        assert(
            !(isCharacter(genes) && isTRUE(isPositive(ntop))),
            msg = "Specify either 'genes' or 'ntop'."
        )
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
        if (isCharacter(genes) || isTRUE(isPositive(ntop))) {
            ## FIXME Need to handle NA gene symbols here.
            ## FIXME Need to update AcidExperiment to handle this better.
            dds <- convertGenesToSymbols(dds)
            rownames(res) <- rownames(dds)
        }
        res <- .prepareResultsForPlot(
            object = res,
            direction = direction,
            alphaThreshold = alphaThreshold,
            baseMeanThreshold = baseMeanThreshold,
            lfcThreshold = lfcThreshold
        )
        if (!hasRows(res)) {
            return(invisible(NULL))
        }
        dds <- as(object, "DESeqDataSet")
        dds <- dds[
            rownames(res),
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
        res <- res[rownames(counts), , drop = FALSE]

        ## FIXME How to apply gene labeling here?

        ## FIXME Need to label DEGs here....
        ## FIXME Use "isDEG" column here (see plotMA code).
        data <- data.frame(
            ## FIXME Need to rework this...
            "x" = rowMeans(
                counts[, contrastMeta[["samples"]][["denominator"]]]
                ),
            "y" = rowMeans(
                counts[, contrastMeta[["samples"]][["numerator"]]]
            ),
            "isDeg" = res[["isDeg"]],
            row.names = rownames(counts)
        )
        p <- ggplot(
            data = data,
            mapping = aes(
                x = !!sym("x"),
                y = !!sym("y")
                ## fill = !!sym("isDEG")
            )
        ) +
            geom_point(size = 1L)
        return(p)
    }



#' @rdname plotContrastScatter
#' @export
setMethod(
    f = "plotContrastScatter",
    signature = signature(object = "DESeqAnalysis"),
    definition = `plotContrastScatter,DESeqAnalysis`
)
