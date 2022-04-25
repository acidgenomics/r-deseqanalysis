## NOTE Consider adding deprecated fallback support for passing of `results` as
## primary argument instead of `object`, which is still in use in bcbioRNASeq
## F1000 v2 paper.



#' @name plotDEGHeatmap
#' @inherit AcidGenerics::plotDEGHeatmap
#' @note Updated 2022-04-15.
#'
#' @inheritParams AcidPlots::plotHeatmap
#' @inheritParams AcidRoxygen::params
#' @inheritParams params
#' @param ... Additional arguments.
#'
#' @param title `logical(1)`, `character(1)`.
#' Include contrast name as title?
#' Can manually define as `character`.
#'
#' @param subtitle `logical(1)`.
#' Include subtitle containing DEG information?
#'
#' @examples
#' data(deseq)
#'
#' ## DESeqAnalysis ====
#' plotDEGHeatmap(deseq, i = 1L)
NULL



## Updated 2021-08-02.
`plotDEGHeatmap,DESeqAnalysis` <- # nolint
    function(object,
             i,
             contrastSamples = FALSE,
             alphaThreshold = NULL,
             baseMeanThreshold = NULL,
             lfcThreshold = NULL,
             ...) {
        validObject(object)
        assert(isFlag(contrastSamples))
        res <- results(object, i = i, quiet = TRUE)
        dt <- as(object, "DESeqTransform")
        if (isTRUE(contrastSamples)) {
            samples <- contrastSamples(object, i = i, return = "character")
            assert(isSubset(samples, colnames(dt)))
            dt <- dt[, samples, drop = FALSE]
            dt <- droplevels2(dt)
        }
        plotDEGHeatmap(
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



## This method is used in F1000 paper and needs to be included. Note that in
## newer versions of bcbioRNASeq, this step won't work because we've slotted the
## rlog/vst counts in as a matrix instead of DESeqTransform.
## Updated 2021-03-15.
`plotDEGHeatmap,DESeqResults` <- # nolint
    function(object,
             DESeqTransform, # nolint
             direction = c("both", "up", "down"),
             alphaThreshold = NULL,
             baseMeanThreshold = NULL,
             lfcThreshold = NULL,
             title = TRUE,
             subtitle = TRUE,
             ...) {
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
            identical(rownames(res), rownames(dt)),
            isFlag(title) || isCharacter(title) || is.null(title),
            isFlag(subtitle)
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
                fmt = "Fewer than %s DEGs to plot. Skipping.",
                .minDEGThreshold
            ))
            return(invisible(NULL))
        }
        ## Subset to only include the DEGs.
        dt <- dt[deg, , drop = FALSE]
        ## Title.
        if (isTRUE(title)) {
            title <- contrastName(res, format = "title")
        } else if (identical(title, FALSE)) {
            title <- NULL
        }
        if (isString(title) && isTRUE(subtitle)) {
            subtitle <- .thresholdLabel(
                object = object,
                direction = direction,
                alphaThreshold = alphaThreshold,
                baseMeanThreshold = baseMeanThreshold,
                lfcShrinkType = lfcShrinkType,
                lfcThreshold = lfcThreshold
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



#' @describeIn plotDEGHeatmap Passes to `DESeqResults` method.
#' @export
setMethod(
    f = "plotDEGHeatmap",
    signature = signature(object = "DESeqAnalysis"),
    definition = `plotDEGHeatmap,DESeqAnalysis`
)

#' @describeIn plotDEGHeatmap Passes to `plotHeatmap()` `SummarizedExperiment`
#' method defined in AcidPlots.
#' @export
setMethod(
    f = "plotDEGHeatmap",
    signature = signature(object = "DESeqResults"),
    definition = `plotDEGHeatmap,DESeqResults`
)
