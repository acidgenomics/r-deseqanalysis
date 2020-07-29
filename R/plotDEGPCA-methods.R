#' @name plotDEGPCA
#' @inherit acidgenerics::plotDEGPCA
#' @note Updated 2020-07-29.
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



## Updated 2020-07-29.
`plotDEGPCA,DESeqResults` <-  # nolint
    function(
        object,
        DESeqTransform,  # nolint
        alpha = NULL,
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
        if (is.null(alpha)) {
            alpha <- metadata(res)[["alpha"]]
        }
        if (is.null(lfcThreshold)) {
            lfcThreshold <- metadata(res)[["lfcThreshold"]]
        }
        if (is.null(baseMeanThreshold)) {
            baseMeanThreshold <- 0L
        }
        assert(
            is(res, "DESeqResults"),
            is(dt, "DESeqTransform"),
            identical(rownames(res), rownames(dt)),
            isAlpha(alpha),
            isNumber(lfcThreshold),
            isNonNegative(lfcThreshold),
            isNumber(baseMeanThreshold),
            isNonNegative(baseMeanThreshold)
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
        ## Titles.
        title <- contrastName(res)
        sep <- "; "
        subtitle <- paste0(
            length(deg), " genes", sep,
            "direction: ", direction, sep,
            "alpha < ", alpha
        )
        if (lfcThreshold > 0L) {
            subtitle <- paste0(
                subtitle, sep,
                "lfc >=", lfcThreshold
            )
        }
        if (baseMeanThreshold > 0L) {
            subtitle <- paste0(
                subtitle, sep,
                "baseMean >=", baseMeanThreshold
            )
        }
        ## Using SummarizedExperiment method here.
        args <- list(
            object = as(dt, "RangedSummarizedExperiment"),
            ntop = Inf,
            title = title,
            subtitle = subtitle
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



## Updated 2019-11-19.
`plotDEGPCA,DESeqAnalysis` <-  # nolint
    function(
        object,
        i,
        contrastSamples = FALSE,
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
