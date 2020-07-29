#' @name plotDEGHeatmap
#' @inherit acidgenerics::plotDEGHeatmap
#' @note Updated 2020-07-28.
#'
#' @inheritParams acidplots::plotHeatmap
#' @inheritParams acidroxygen::params
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



#' @rdname plotDEGHeatmap
#' @name plotDEGHeatmap
#' @importFrom acidgenerics plotDEGHeatmap
#' @usage plotDEGHeatmap(object, ...)
#' @export
NULL



## This method is used in F1000 paper and needs to be included. Note that in
## newer versions of bcbioRNASeq, this step won't work because we've slotted the
## rlog/vst counts in as a matrix instead of DESeqTransform.
## Updated 2020-07-28.
`plotDEGHeatmap,DESeqResults` <-  # nolint
    function(
        object,
        DESeqTransform,  # nolint
        alpha = NULL,
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
        if (is.null(alpha)) {
            alpha <- metadata(res)[["alpha"]]
        }
        if (is.null(lfcThreshold)) {
            lfcThreshold <- metadata(res)[["lfcThreshold"]]
        }
        lfcShrinkType <- lfcShrinkType(object)
        if (is.null(baseMeanThreshold)) {
            baseMeanThreshold <- 0L
        }
        assert(
            is(res, "DESeqResults"),
            is(dt, "DESeqTransform"),
            identical(rownames(object), rownames(DESeqTransform)),
            isFlag(title) || isCharacter(title) || is.null(title),
            isFlag(subtitle)
        )
        direction <- match.arg(direction)
        deg <- deg(
            object = res,
            alpha = alpha,
            lfcThreshold = lfcThreshold,
            baseMeanThreshold = baseMeanThreshold,
            direction = direction
        )
        if (length(deg) < .minDEGThreshold) {
            message(sprintf(
                fmt = "Fewer than %s DEGs to plot. Skipping.",
                .minDEGThreshold
            ))
            return()
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
            sep <- "; "
            title <- paste0(
                title, "\n",
                length(deg), " genes", sep,
                "direction: ", direction, sep,
                "alpha < ", alpha
            )
            if (lfcThreshold > 0L) {
                title <- paste0(
                    title, sep,
                    "lfc >= ", lfcThreshold
                )
            }
            if (lfcShrinkType != "unshrunken") {
                title <- paste0(
                    title, sep,
                    "lfcShrink: ", lfcShrinkType
                )
            }
            if (baseMeanThreshold > 0L) {
                title <- paste0(
                    title, sep,
                    "baseMean >= ", baseMeanThreshold
                )
            }
        }
        ## Using SummarizedExperiment method defined in acidplots here.
        args <- list(
            object = as(dt, "RangedSummarizedExperiment"),
            title = title
        )
        args <- c(args, list(...))
        do.call(what = plotHeatmap, args = args)
    }



#' @describeIn plotDEGHeatmap Passes to `plotHeatmap()` `SummarizedExperiment`
#'   method defined in acidplots.
#' @export
setMethod(
    f = "plotDEGHeatmap",
    signature = signature("DESeqResults"),
    definition = `plotDEGHeatmap,DESeqResults`
)



## Updated 2019-11-08.
`plotDEGHeatmap,DESeqAnalysis` <-  # nolint
    function(
        object,
        i,
        contrastSamples = FALSE,
        lfcShrink = TRUE,
        ...
    ) {
        ## nocov start
        call <- match.call()
        ## results
        if ("results" %in% names(call)) {
            stop("'results' is defunct in favor of 'i'.")
        }
        rm(call)
        ## nocov end
        validObject(object)
        assert(
            isScalar(i),
            isFlag(contrastSamples),
            isFlag(lfcShrink)
        )
        ## Note use of `res` here instead of `results`, since we need to check
        ## the original `results` input below in `contrastSamples()` call.
        res <- results(object, i = i, lfcShrink = lfcShrink)
        ## We're using the variance-stabilized counts for visualization here.
        dt <- as(object, "DESeqTransform")
        ## Subset the DESeqTransform, if necessary.
        if (isTRUE(contrastSamples)) {
            samples <- contrastSamples(object, i = i)
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
