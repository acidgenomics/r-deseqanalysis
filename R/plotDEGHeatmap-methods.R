# NOTE: `clusterRows = TRUE` can fail for some datasets.
# Use a `tryCatch` call here to attempt to detect.

# Error in hclust(d, method = method) :
# NA/NaN/Inf in foreign function call (arg 11)
# Calls: plotDEGHeatmap ... do.call -> do.call -> <Anonymous> -> cluster_mat -> hclust

# Works:
# plotDEGHeatmap(MCF7_analysis, results = 2L, contrastSamples = TRUE, clusterRows = FALSE)
#
# Errors:
# plotDEGHeatmap(MCF7_analysis, results = 2L, contrastSamples = TRUE, clusterRows = FALSE)



# Do not allow post hoc alpha or lfcThreshold cutoffs here.



#' @name plotDEGHeatmap
#' @inherit basejump::plotHeatmap
#' @inheritParams basejump::params
#' @inheritParams params
#'
#' @examples
#' data(deseq)
#'
#' ## DESeqAnalysis ====
#' plotDEGHeatmap(deseq)
NULL



#' @importFrom basejump plotDEGHeatmap
#' @aliases NULL
#' @export
basejump::plotDEGHeatmap



plotDEGHeatmap.DESeqAnalysis <-  # nolint
    function(
        object,
        results = 1L,
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

        results <- .matchResults(object, results)
        validObject(results)

        counts <- as(object, "DESeqTransform")
        validObject(counts)

        assert(identical(rownames(results), rownames(counts)))

        interestingGroups(counts) <-
            matchInterestingGroups(counts, interestingGroups)

        alpha <- metadata(results)[["alpha"]]
        assert(containsAlpha(alpha))

        lfcThreshold <- metadata(results)[["lfcThreshold"]]
        assert(
            isNumber(lfcThreshold),
            isNonNegative(lfcThreshold)
        )

        # Get the character vector of DEGs.
        deg <- deg(object = results, direction = direction)
        if (!hasLength(deg)) {
            return(invisible())
        }

        se <- counts %>%
            as("RangedSummarizedExperiment") %>%
            as("SummarizedExperiment") %>%
            .[deg, , drop = FALSE]

        # Subset the counts to match contrast samples, if desired.
        if (isTRUE(contrastSamples)) {
            samples <- contrastSamples(object)
            assert(isSubset(samples, colnames(se)))
            se <- se[, samples, drop = FALSE]
            colData(se) <- relevelColData(colData(se))
        }

        # Title
        title <- paste0(
            contrastName(results), "\n",
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
                    object = se,
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
