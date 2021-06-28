#' @name plotDEGPCA
#' @inherit AcidGenerics::plotDEGPCA
#' @note Updated 2021-03-03.
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



## Updated 2021-03-15.
`plotDEGPCA,DESeqAnalysis` <-  # nolint
    function(
        object,
        i,
        contrastSamples = FALSE,
        alphaThreshold = NULL,
        lfcThreshold = NULL,
        baseMeanThreshold = NULL,
        ...
    ) {
        assert(isFlag(contrastSamples))
        res <- results(object, i = i, quiet = TRUE)
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
            alphaThreshold = ifelse(
                test = is.null(alphaThreshold),
                yes = alphaThreshold(object),
                no = alphaThreshold
            ),
            lfcThreshold = ifelse(
                test = is.null(lfcThreshold),
                yes = lfcThreshold(object),
                no = lfcThreshold
            ),
            baseMeanThreshold = ifelse(
                test = is.null(baseMeanThreshold),
                yes = baseMeanThreshold(object),
                no = baseMeanThreshold
            ),
            ...
        )
    }



## Updated 2021-03-03.
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
                ## FIXME Rework this using metadata stash approach?
                subtitle = .thresholdLabel(
                    object = object,
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



#' @describeIn plotDEGPCA Passes to `DESeqResults` method.
#' @export
setMethod(
    f = "plotDEGPCA",
    signature = signature("DESeqAnalysis"),
    definition = `plotDEGPCA,DESeqAnalysis`
)



#' @rdname plotDEGPCA
#' @export
setMethod(
    f = "plotDEGPCA",
    signature = signature("DESeqResults"),
    definition = `plotDEGPCA,DESeqResults`
)
