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
        baseMeanThreshold = NULL,
        lfcThreshold = NULL,
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
            baseMeanThreshold = ifelse(
                test = is.null(baseMeanThreshold),
                yes = baseMeanThreshold(object),
                no = baseMeanThreshold
            ),
            lfcThreshold = ifelse(
                test = is.null(lfcThreshold),
                yes = lfcThreshold(object),
                no = lfcThreshold
            ),
            ...
        )
    }



## Updated 2021-03-03.
`plotDEGPCA,DESeqResults` <-  # nolint
    function(
        object,
        DESeqTransform,  # nolint
        direction = c("both", "up", "down"),
        alphaThreshold = NULL,
        baseMeanThreshold = NULL,
        lfcThreshold = NULL,
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
        if (is.null(baseMeanThreshold)) {
            baseMeanThreshold <- baseMeanThreshold(res)
        }
        if (is.null(lfcThreshold)) {
            lfcThreshold <- lfcThreshold(res)
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
            direction = direction,
            alphaThreshold = alphaThreshold,
            baseMeanThreshold = baseMeanThreshold,
            lfcThreshold = lfcThreshold,
            quiet = TRUE
        )
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
                    object = object,
                    direction = direction,
                    alphaThreshold = alphaThreshold,
                    baseMeanThreshold = baseMeanThreshold,
                    lfcShrinkType = lfcShrinkType,
                    lfcThreshold = lfcThreshold
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
