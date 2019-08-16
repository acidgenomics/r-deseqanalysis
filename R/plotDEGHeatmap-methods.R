## Do not allow post hoc alpha or lfcThreshold cutoffs here.



#' @name plotDEGHeatmap
#' @inherit bioverbs::plotDEGHeatmap
#'
#' @inheritParams acidplots::plotHeatmap
#' @inheritParams acidroxygen::params
#' @inheritParams params
#' @param ... Additional arguments.
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
#' @usage plotDEGHeatmap(object, ...)
#' @export
NULL



## This method is used in F1000 paper and needs to be included. Note that in
## newer versions of bcbioRNASeq, this step won't work because we've slotted the
## rlog/vst counts in as a matrix instead of DESeqTransform.
## Updated 2019-07-23.
`plotDEGHeatmap,DESeqResults` <-  # nolint
    function(
        object,
        DESeqTransform,  # nolint
        direction = c("both", "up", "down"),
        scale = c("row", "column", "none"),
        clusteringMethod = "ward.D2",
        clusterRows = TRUE,
        clusterCols = TRUE,
        breaks = seq(from = -2L, to = 2L, by = 0.25),
        legendBreaks = seq(from = -2L, to = 2L, by = 1L)
    ) {
        validObject(object)
        validObject(DESeqTransform)
        assert(
            is(object, "DESeqResults"),
            is(DESeqTransform, "DESeqTransform"),
            identical(rownames(object), rownames(DESeqTransform)),
            isString(clusteringMethod),
            is.numeric(legendBreaks)
        )
        direction <- match.arg(direction)
        scale <- match.arg(scale)

        ## Rename objects internally to make the code more readable.
        res <- object
        dt <- DESeqTransform

        interestingGroups(dt) <- matchInterestingGroups(dt, interestingGroups)

        alpha <- metadata(res)[["alpha"]]
        lfcThreshold <- metadata(res)[["lfcThreshold"]]
        lfcShrinkType <- lfcShrinkType(object)
        assert(
            isAlpha(alpha),
            isNumber(lfcThreshold),
            isNonNegative(lfcThreshold),
            isString(lfcShrinkType)
        )

        ## Get the character vector of DEGs.
        deg <- deg(res, direction = direction)
        if (!hasLength(deg)) {
            warning("There are no DEGs to plot. Skipping.")
            return(invisible())
        }

        ## Subset to only include the DEGs.
        dt <- dt[deg, , drop = FALSE]

        ## Title
        title <- paste0(
            contrastName(res, format = "title"), "\n",
            length(deg), " genes;  ",
            "alpha: ", alpha, ";  ",
            "lfcThreshold: ", lfcThreshold, ";  ",
            "lfcShrink: ", lfcShrinkType
        )
        if (lfcThreshold > 0L) {
            title <- paste0(title, "; lfc > ", lfcThreshold)
        }

        ## Using SummarizedExperiment method defined in basejump here.
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
                    "DESeqTransform",
                    "direction"
                )
            )
        )
    }

f1 <- formals(`plotDEGHeatmap,DESeqResults`)
f2 <- methodFormals(
    f = "plotHeatmap",
    signature = "SummarizedExperiment",
    package = "acidplots"
)
f2 <- f2[setdiff(names(f2), c(names(f1), "object", "assay"))]
f <- c(f1, f2)
f[["color"]] <- quote(
    getOption(
        x = "acid.heatmap.color",
        default = acidplots::blueYellow
    )
)
formals(`plotDEGHeatmap,DESeqResults`) <- f



#' @rdname plotDEGHeatmap
#' @export
setMethod(
    f = "plotDEGHeatmap",
    signature = signature("DESeqResults"),
    definition = `plotDEGHeatmap,DESeqResults`
)



## Updated 2019-07-23.
`plotDEGHeatmap,DESeqAnalysis` <-  # nolint
    function(
        object,
        results,
        contrastSamples = FALSE,
        lfcShrink = TRUE
    ) {
        validObject(object)
        assert(
            isFlag(contrastSamples),
            isFlag(lfcShrink)
        )

        ## Note use of `res` here instead of `results`, since we need to check
        ## the original `results` input below in `contrastSamples()` call.
        res <- results(object, results = results, lfcShrink = lfcShrink)
        validObject(res)

        ## We're using the variance-stabilized counts for visualization here.
        dt <- as(object, "DESeqTransform")
        validObject(dt)

        ## Subset the DESeqTransform, if necessary.
        if (isTRUE(contrastSamples)) {
            samples <- contrastSamples(object, results = results)
            assert(isSubset(samples, colnames(dt)))
            dt <- dt[, samples, drop = FALSE]
            dt <- droplevels(dt)
        }

        ## Passing to DESeqResults/DESeqTransform method.
        do.call(
            what = plotDEGHeatmap,
            args = matchArgsToDoCall(
                args = list(
                    object = res,
                    DESeqTransform = dt
                ),
                removeFormals = c(
                    "results",
                    "contrastSamples",
                    "lfcShrink"
                )
            )
        )
    }

f1 <- formals(`plotDEGHeatmap,DESeqAnalysis`)
f2 <- formals(`plotDEGHeatmap,DESeqResults`)
f2 <- f2[setdiff(names(f2), c(names(f1), "DESeqTransform"))]
f <- c(f1, f2)
formals(`plotDEGHeatmap,DESeqAnalysis`) <- f



#' @rdname plotDEGHeatmap
#' @export
setMethod(
    f = "plotDEGHeatmap",
    signature = signature("DESeqAnalysis"),
    definition = `plotDEGHeatmap,DESeqAnalysis`
)
