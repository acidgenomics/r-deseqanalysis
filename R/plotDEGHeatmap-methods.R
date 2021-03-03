#' @name plotDEGHeatmap
#' @inherit AcidGenerics::plotDEGHeatmap
#' @note Updated 2020-08-05.
#'
#' @inheritParams AcidPlots::plotHeatmap
#' @inheritParams AcidRoxygen::params
#' @inheritParams params
#' @param title `logical(1)`, `character(1)`.
#'   Include contrast name as title?
#'   Can manually define as `character`.
#' @param subtitle `logical(1)`.
#'   Include subtitle containing DEG information?
#' @param ... Additional arguments.
#'
#' @examples
#' data(deseq)
#'
#' ## DESeqAnalysis ====
#' plotDEGHeatmap(deseq, i = 1L)
NULL



## This method is used in F1000 paper and needs to be included. Note that in
## newer versions of bcbioRNASeq, this step won't work because we've slotted the
## rlog/vst counts in as a matrix instead of DESeqTransform.
## Updated 2020-08-04.
`plotDEGHeatmap,DESeqResults` <-  # nolint
    function(
        object,
        DESeqTransform,  # nolint
        alphaThreshold = NULL,
        lfcThreshold = NULL,
        baseMeanThreshold = NULL,
        direction = c("both", "up", "down"),
        title = TRUE,
        subtitle = TRUE,
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
            identical(rownames(res), rownames(dt)),
            isFlag(title) || isCharacter(title) || is.null(title),
            isFlag(subtitle)
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
                fmt = "Fewer than %s DEGs to plot. Skipping.",
                .minDEGThreshold
            ))
            return(invisible(NULL))
        }
        ## Subset to only include the DEGs.
        dt <- dt[deg, , drop = FALSE]
        ## Title.
        if (isTRUE(title)) {
            title <- contrastName(res, .format = "title")
        } else if (identical(title, FALSE)) {
            title <- NULL
        }
        if (isString(title) && isTRUE(subtitle)) {
            ## FIXME REWORK THIS.
            subtitle <- .thresholdLabel(
                n = length(deg),
                direction = direction,
                alphaThreshold = alphaThreshold,
                lfcShrinkType = lfcShrinkType,
                lfcThreshold = lfcThreshold,
                baseMeanThreshold = baseMeanThreshold
            )
            title <- paste(title, subtitle, sep = "\n")
        }
        ## Using SummarizedExperiment method defined in AcidPlots here.
        args <- list(
            object = as(dt, "RangedSummarizedExperiment"),
            title = title
        )
        args <- c(args, list(...))
        do.call(what = plotHeatmap, args = args)
    }



#' @describeIn plotDEGHeatmap Passes to `plotHeatmap()` `SummarizedExperiment`
#'   method defined in AcidPlots.
#' @export
setMethod(
    f = "plotDEGHeatmap",
    signature = signature("DESeqResults"),
    definition = `plotDEGHeatmap,DESeqResults`
)



## Updated 2020-08-25.
`plotDEGHeatmap,DESeqAnalysis` <-  # nolint
    function(object, i, contrastSamples = FALSE, ...) {
        validObject(object)
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
        plotDEGHeatmap(
            object = res,
            DESeqTransform = dt,
            alphaThreshold = alphaThreshold(object),
            lfcThreshold = lfcThreshold(object),
            baseMeanThreshold = baseMeanThreshold(object),
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
