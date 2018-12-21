# Do not allow post hoc alpha or lfcThreshold cutoffs here.



#' @name plotDEGHeatmap
#' @inherit bioverbs::plotDEGHeatmap
#' @inheritParams basejump::plotHeatmap
#' @inheritParams params
#'
#' @examples
#' data(deseq)
#'
#' ## DESeqAnalysis ====
#' plotDEGHeatmap(deseq, results = 1L)
NULL



#' @importFrom bioverbs plotDEGHeatmap
#' @aliases NULL
#' @export
bioverbs::plotDEGHeatmap



plotDEGHeatmap.DESeqAnalysis <-  # nolint
    function(
        object,
        results,
        contrastSamples = FALSE,
        direction = c("both", "up", "down"),
        scale = c("row", "column", "none"),
        clusteringMethod = "ward.D2",
        clusterRows = TRUE,
        clusterCols = TRUE
    ) {
        validObject(object)
        assert(
            isFlag(contrastSamples),
            isString(clusteringMethod)
        )
        direction <- match.arg(direction)
        scale <- match.arg(scale)

        res <- .matchResults(object, results)
        validObject(res)

        # We're using the variance-stabilized counts for visualization.
        dt <- as(object, "DESeqTransform")
        validObject(dt)

        assert(identical(rownames(res), rownames(dt)))

        interestingGroups(dt) <-
            matchInterestingGroups(dt, interestingGroups)

        alpha <- metadata(res)[["alpha"]]
        assert(isAlpha(alpha))

        lfcThreshold <- metadata(res)[["lfcThreshold"]]
        assert(
            isNumber(lfcThreshold),
            isNonNegative(lfcThreshold)
        )

        # Get the character vector of DEGs.
        deg <- deg(res, direction = direction)
        if (!hasLength(deg)) {
            warning("There are no DEGs to plot. Skipping.", call. = FALSE)
            return(invisible())
        }

        # Subset to only include the DEGs.
        dt <- dt[deg, , drop = FALSE]

        if (isTRUE(contrastSamples)) {
            samples <- contrastSamples(object, results = results)
            assert(isSubset(samples, colnames(dt)))
            dt <- dt[, samples, drop = FALSE]
            colData(dt) <- relevelColData(colData(dt))
        }

        # Title
        title <- paste0(
            contrastName(res), "\n",
            length(deg), " genes; ",
            "alpha < ", alpha
        )
        if (lfcThreshold > 0L) {
            title <- paste0(title, "; lfc > ", lfcThreshold)
        }

        # Using SummarizedExperiment method here.
        do.call(
            what = plotHeatmap,
            args = matchArgsToDoCall(
                args = list(
                    object = as(dt, "RangedSummarizedExperiment"),
                    scale = scale,
                    title = title
                ),
                removeFormals = c(
                    "alpha",
                    "contrastSamples",
                    "counts",
                    "direction",
                    "lfcThreshold",
                    "results"
                )
            )
        )
    }

f1 <- formals(plotDEGHeatmap.DESeqAnalysis)
f2 <- methodFormals(
    f = "plotHeatmap",
    signature = "SummarizedExperiment",
    package = "basejump"
)
f2 <- f2[setdiff(names(f2), c(names(f1), "object", "assay"))]
f <- c(f1, f2)
formals(plotDEGHeatmap.DESeqAnalysis) <- f



#' @rdname plotDEGHeatmap
#' @export
setMethod(
    f = "plotDEGHeatmap",
    signature = signature("DESeqAnalysis"),
    definition = plotDEGHeatmap.DESeqAnalysis
)
