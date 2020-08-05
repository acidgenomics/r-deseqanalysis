#' @name plotDEGPCA
#' @inherit acidgenerics::plotDEGPCA
#' @note Updated 2020-08-04.
#'
#' @inheritParams plotDEGHeatmap
#' @inheritParams acidplots::plotPCA
#' @inheritParams acidroxygen::params
#' @inheritParams params
#' @param ... Additional arguments.
#'
#' @examples
#' data(deseq)
#'
#' ## DESeqAnalysis ====
#' plotDEGPCA(deseq, i = 1L)
NULL



#' @rdname plotDEGPCA
#' @name plotDEGPCA
#' @importFrom acidgenerics plotDEGPCA
#' @usage plotDEGPCA(object, ...)
#' @export
NULL



## Updated 2020-08-04.
`plotDEGPCA,DESeqResults` <-  # nolint
    function(
        object,
        DESeqTransform,  # nolint
        alphaThreshold = NULL,
        lfcThreshold = NULL,
        baseMeanThreshold = NULL,
        direction = c("both", "up", "down"),
        ...
    ) {
        validObject(object)
        validObject(DESeqTransform)
        ## Rename objects internally to make the code more readable.
        res <- object
        dt <- DESeqTransform
        interestingGroups(dt) <-
            matchInterestingGroups(dt, interestingGroups)
        if (is.null(alphaThreshold)) {
            alphaThreshold <- alphaThreshold(res)
        }
        if (is.null(lfcThreshold)) {
            lfcThreshold <- lfcThreshold(res)
        }
        if (is.null(baseMeanThreshold)) {
            baseMeanThreshold <- baseMeanThreshold(res)
        }
        lfcShrinkType <- lfcShrinkType(res)
        assert(
            is(res, "DESeqResults"),
            is(dt, "DESeqTransform"),
            identical(rownames(res), rownames(dt))
        )
        direction <- match.arg(direction)
        deg <- deg(
            object = res,
            alphaThreshold = alphaThreshold,
            lfcThreshold = lfcThreshold,
            baseMeanThreshold = baseMeanThreshold,
            direction = direction
        )
        if (length(deg) < .minDEGThreshold) {
            cli_alert_warning(sprintf(
                fmt = "Fewer than %s DEG to plot. Skipping.",
                .minDEGThreshold
            ))
            return()
        }
        ## Subset to only include the DEGs.
        dt <- dt[deg, , drop = FALSE]
        ## Using SummarizedExperiment method here.
        args <- list(
            object = as(dt, "RangedSummarizedExperiment"),
            ntop = Inf,
            title = contrastName(res),
            subtitle = .thresholdLabel(
                n = length(deg),
                direction = direction,
                alphaThreshold = alphaThreshold,
                lfcShrinkType = lfcShrinkType,
                lfcThreshold = lfcThreshold,
                baseMeanThreshold = baseMeanThreshold
            )
        )
        args <- c(args, list(...))
        do.call(what = plotPCA, args = args)
    }



#' @rdname plotDEGPCA
#' @export
setMethod(
    f = "plotDEGPCA",
    signature = signature("DESeqResults"),
    definition = `plotDEGPCA,DESeqResults`
)



## Updated 2020-08-04.
`plotDEGPCA,DESeqAnalysis` <-  # nolint
    function(
        object,
        i,
        contrastSamples = FALSE,
        ...
    ) {
        validObject(object)
        assert(
            isScalar(i),
            isFlag(contrastSamples)
        )
        ## Note that LFC values aren't used for this plot, just the DEGs, which
        ## are used to subset the DESeqTransform counts.
        res <- results(object, i = i, lfcShrink = FALSE)
        ## Using the variance-stabilized counts for visualization.
        dt <- as(object, "DESeqTransform")
        ## Subset the DESeqTransform, if necessary.
        if (isTRUE(contrastSamples)) {
            samples <- contrastSamples(object, i = i)
            assert(isSubset(samples, colnames(dt)))
            dt <- dt[, samples, drop = FALSE]
            dt <- droplevels(dt)
        }
        ## Passing through to DESeqResults/DESeqTransform method here.
        plotDEGPCA(object = res, DESeqTransform = dt, ...)
    }



#' @describeIn plotDEGPCA Passes to `DESeqResults` method.
#' @export
setMethod(
    f = "plotDEGPCA",
    signature = signature("DESeqAnalysis"),
    definition = `plotDEGPCA,DESeqAnalysis`
)
