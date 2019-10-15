## Do not allow post hoc alpha or lfcThreshold cutoffs here.



#' @name plotDEGHeatmap
#' @inherit bioverbs::plotDEGHeatmap
#' @note Updated 2019-10-15.
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
## Updated 2019-10-15.
`plotDEGHeatmap,DESeqResults` <-  # nolint
    function(
        object,
        DESeqTransform,  # nolint
        interestingGroups = NULL,
        direction = c("both", "up", "down"),
        scale = c("row", "column", "none"),
        clusteringMethod = "ward.D2",
        clusterRows = TRUE,
        clusterCols = TRUE,
        color,
        breaks = seq(from = -2L, to = 2L, by = 0.25),
        legendBreaks = seq(from = -2L, to = 2L, by = 1L),
        ...
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
        if (length(deg) < .minDEGThreshold) {
            message(sprintf(
                fmt = "Fewer than %s DEGs to plot. Skipping.",
                .minDEGThreshold
            ))
            return(invisible())
        }
        ## Subset to only include the DEGs.
        dt <- dt[deg, , drop = FALSE]
        ## Title.
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
        args <- list(
            object = as(dt, "RangedSummarizedExperiment"),
            scale = scale,
            clusteringMethod = clusteringMethod,
            clusterRows = clusterRows,
            clusterCols = clusterCols,
            color = color,
            breaks = breaks,
            legendBreaks = legendBreaks
        )
        args <- c(args, list(...))
        do.call(what = plotHeatmap, args = args)
    }

formals(`plotDEGHeatmap,DESeqResults`)[["color"]] <-
    quote(
        getOption(
            x = "acid.heatmap.color",
            default = acidplots::blueYellow
        )
    )



#' @describeIn plotDEGHeatmap Passes to `plotHeatmap()` `SummarizedExperiment`
#'   method defined in acidplots.
#' @export
setMethod(
    f = "plotDEGHeatmap",
    signature = signature("DESeqResults"),
    definition = `plotDEGHeatmap,DESeqResults`
)



## Updated 2019-10-15.
`plotDEGHeatmap,DESeqAnalysis` <-  # nolint
    function(
        object,
        results,
        contrastSamples = FALSE,
        lfcShrink = TRUE,
        ...
    ) {
        validObject(object)
        assert(
            isFlag(contrastSamples),
            isFlag(lfcShrink)
        )
        ## Note use of `res` here instead of `results`, since we need to check
        ## the original `results` input below in `contrastSamples()` call.
        res <- results(object, results = results, lfcShrink = lfcShrink)
        ## We're using the variance-stabilized counts for visualization here.
        dt <- as(object, "DESeqTransform")
        ## Subset the DESeqTransform, if necessary.
        if (isTRUE(contrastSamples)) {
            samples <- contrastSamples(object, results = results)
            assert(isSubset(samples, colnames(dt)))
            dt <- dt[, samples, drop = FALSE]
            dt <- droplevels(dt)
        }
        ## Passing to DESeqResults/DESeqTransform method.
        plotDEGHeatmap(
            object = res,
            DESeqTransform = dt,
            ...
        )
    }



#' @describeIn plotDEGHeatmap Passes to `DESeqResults` method.
#' @export
setMethod(
    f = "plotDEGHeatmap",
    signature = signature("DESeqAnalysis"),
    definition = `plotDEGHeatmap,DESeqAnalysis`
)
