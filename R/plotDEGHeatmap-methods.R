# Do not allow post hoc alpha or lfcThreshold cutoffs here.



#' @name plotDEGHeatmap
#' @inherit bioverbs::plotDEGHeatmap
#' @inheritParams firestarter::plotHeatmap
#' @inheritParams basejump::params
#' @inheritParams params
#'
#' @param counts `DESeqTransform`.
#'   Variance-stabilized counts suitable for heatmap.
#'   Object rownames must be identical to corresponding `DESeqResults`.
#'
#' @examples
#' data(deseq)
#'
#' ## DESeqAnalysis ====
#' plotDEGHeatmap(deseq, results = 1L)
NULL



#' @rdname plotDEGHeatmap
#' @name plotDEGHeatmap
#' @importFrom bioverbs plotDEGHeatmap
#' @export
NULL



# This method is used in F1000 paper and needs to be included. Note that in
# newer versions of bcbioRNASeq, this step won't work because we've slotted the
# rlog/vst counts in as a matrix instead of DESeqTransform.
plotDEGHeatmap.DESeqResults <-  # nolint
    function(
        object,
        counts,
        direction = c("both", "up", "down"),
        scale = c("row", "column", "none"),
        clusteringMethod = "ward.D2",
        clusterRows = TRUE,
        clusterCols = TRUE
    ) {
        validObject(object)
        validObject(counts)
        assert(
            is(object, "DESeqResults"),
            is(counts, "DESeqTransform"),
            identical(rownames(object), rownames(counts)),
            isString(clusteringMethod)
        )
        direction <- match.arg(direction)
        scale <- match.arg(scale)

        # Rename objects internally to make the code more readable.
        res <- object
        dt <- counts

        interestingGroups(dt) <- matchInterestingGroups(dt, interestingGroups)
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

        # Title
        title <- paste0(
            contrastName(res), "\n",
            length(deg), " genes; ",
            "alpha < ", alpha
        )
        if (lfcThreshold > 0L) {
            title <- paste0(title, "; lfc > ", lfcThreshold)
        }

        # Using SummarizedExperiment method defined in basejump here.
        rse <- as(dt, "RangedSummarizedExperiment")
        do.call(
            what = plotHeatmap,
            args = matchArgsToDoCall(
                args = list(
                    object = rse,
                    scale = scale,
                    title = title
                ),
                removeFormals = c(
                    "counts",
                    "direction"
                )
            )
        )
    }

f1 <- formals(plotDEGHeatmap.DESeqResults)
f2 <- methodFormals(
    f = "plotHeatmap",
    signature = "SummarizedExperiment",
    package = "firestarter"
)
f2 <- f2[setdiff(names(f2), c(names(f1), "object", "assay"))]
f <- c(f1, f2)
formals(plotDEGHeatmap.DESeqResults) <- f



#' @rdname plotDEGHeatmap
#' @export
setMethod(
    f = "plotDEGHeatmap",
    signature = signature("DESeqResults"),
    definition = plotDEGHeatmap.DESeqResults
)



plotDEGHeatmap.DESeqAnalysis <-  # nolint
    function(
        object,
        results,
        contrastSamples = FALSE
    ) {
        validObject(object)
        assert(isFlag(contrastSamples))

        res <- .matchResults(object, results)
        validObject(res)

        # We're using the variance-stabilized counts for visualization here.
        dt <- as(object, "DESeqTransform")
        validObject(dt)

        # Subset the DESeqTransform, if necessary.
        if (isTRUE(contrastSamples)) {
            samples <- contrastSamples(object, results = results)
            assert(isSubset(samples, colnames(dt)))
            dt <- dt[, samples, drop = FALSE]
            colData(dt) <- relevelColData(colData(dt))
        }

        # Passing to DESeqResults/DESeqTransform method.
        do.call(
            what = plotDEGHeatmap,
            args = matchArgsToDoCall(
                args = list(
                    object = res,
                    counts = dt
                ),
                removeFormals = c(
                    "results",
                    "contrastSamples"
                )
            )
        )
    }

f1 <- formals(plotDEGHeatmap.DESeqAnalysis)
f2 <- formals(plotDEGHeatmap.DESeqResults)
f2 <- f2[setdiff(names(f2), c(names(f1), "counts"))]
f <- c(f1, f2)
formals(plotDEGHeatmap.DESeqAnalysis) <- f



#' @rdname plotDEGHeatmap
#' @export
setMethod(
    f = "plotDEGHeatmap",
    signature = signature("DESeqAnalysis"),
    definition = plotDEGHeatmap.DESeqAnalysis
)
