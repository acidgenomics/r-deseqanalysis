#' @name plotDegPca
#' @inherit AcidGenerics::plotDegPca
#' @note Updated 2021-08-02.
#'
#' @inheritParams plotDegHeatmap
#' @inheritParams AcidPlots::plotPca
#' @inheritParams AcidRoxygen::params
#' @inheritParams params
#' @param ... Additional arguments.
#'
#' @examples
#' data(deseq)
#'
#' ## DESeqAnalysis ====
#' plotDegPca(deseq, i = 1L)
NULL



## Updated 2021-03-15.
`plotDegPca,DESeqAnalysis` <- # nolint
    function(object,
             i,
             contrastSamples = FALSE,
             alphaThreshold = NULL,
             baseMeanThreshold = NULL,
             lfcThreshold = NULL,
             ...) {
        assert(isFlag(contrastSamples))
        res <- results(object, i = i, quiet = TRUE)
        dt <- as(object, "DESeqTransform")
        if (isTRUE(contrastSamples)) {
            samples <- contrastSamples(object, i = i, return = "character")
            assert(isSubset(samples, colnames(dt)))
            dt <- dt[, samples, drop = FALSE]
            dt <- droplevels2(dt)
        }
        plotDegPca(
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



## Updated 2023-12-18.
`plotDegPca,DESeqResults` <- # nolint
    function(object,
             DESeqTransform, # nolint
             direction = c("both", "up", "down"),
             alphaThreshold = NULL,
             baseMeanThreshold = NULL,
             lfcThreshold = NULL,
             ...) {
        ## Disabling this check to support legacy objects.
        ## > assert(
        ## >     validObject(object),
        ## >     validObject(DESeqTransform)
        ## > )
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
        do.call(what = plotPca, args = args)
    }



#' @describeIn plotDegPca Passes to `DESeqResults` method.
#' @export
setMethod(
    f = "plotDegPca",
    signature = signature(object = "DESeqAnalysis"),
    definition = `plotDegPca,DESeqAnalysis`
)

#' @rdname plotDegPca
#' @export
setMethod(
    f = "plotDegPca",
    signature = signature(object = "DESeqResults"),
    definition = `plotDegPca,DESeqResults`
)
