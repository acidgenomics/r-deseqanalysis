#' @name plotDEGPCA
#' @inherit AcidGenerics::plotDEGPCA
#' @note Updated 2020-08-25.
#'
#' @inheritParams plotDEGHeatmap
#' @inheritParams AcidPlots::plotPCA
#' @inheritParams AcidRoxygen::params
#' @inheritParams params
#' @param ... Additional arguments.
#'
#' @examples
#' data(deseq)
#'
#' ## DESeqAnalysis ====
#' plotDEGPCA(deseq, i = 1L)
NULL



## Updated 2020-08-25.
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
        suppressMessages({
            deg <- deg(
                object = res,
                alphaThreshold = alphaThreshold,
                lfcThreshold = lfcThreshold,
                baseMeanThreshold = baseMeanThreshold,
                direction = direction
            )
        })
        if (length(deg) < .minDEGThreshold) {
            alertWarning(sprintf(
                fmt = "Fewer than %s DEG to plot. Skipping.",
                .minDEGThreshold
            ))
            return(invisible(NULL))
        }
        ## Subset to only include the DEGs.
        dt <- dt[deg, , drop = FALSE]
        ## Using SummarizedExperiment method here.
        args <- list(
            object = as(dt, "RangedSummarizedExperiment"),
            ntop = Inf,
            labels = list(
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



## Updated 2020-08-25.
`plotDEGPCA,DESeqAnalysis` <-  # nolint
    function(object, i, contrastSamples = FALSE, ...) {
        assert(isFlag(contrastSamples))
        suppressMessages({
            res <- results(object, i = i)
        })
        dt <- as(object, "DESeqTransform")
        if (isTRUE(contrastSamples)) {
            samples <- contrastSamples(object, i = i)
            assert(isSubset(samples, colnames(dt)))
            dt <- dt[, samples, drop = FALSE]
            dt <- droplevels(dt)
        }
        plotDEGPCA(
            object = res,
            DESeqTransform = dt,
            alphaThreshold = alphaThreshold(object),
            lfcThreshold = lfcThreshold(object),
            baseMeanThreshold = baseMeanThreshold(object),
            ...
        )
    }



#' @describeIn plotDEGPCA Passes to `DESeqResults` method.
#' @export
setMethod(
    f = "plotDEGPCA",
    signature = signature("DESeqAnalysis"),
    definition = `plotDEGPCA,DESeqAnalysis`
)
