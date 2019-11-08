## Do not allow post hoc alpha or lfcThreshold cutoffs here.



#' @name plotDEGPCA
#' @inherit bioverbs::plotDEGPCA
#' @note Updated 2019-11-08.
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
#' @importFrom bioverbs plotDEGPCA
#' @usage plotDEGPCA(object, ...)
#' @export
NULL



## Updated 2019-10-15.
`plotDEGPCA,DESeqResults` <-  # nolint
    function(
        object,
        DESeqTransform,  # nolint
        interestingGroups = NULL,
        direction = c("both", "up", "down"),
        ...
    ) {
        validObject(object)
        validObject(DESeqTransform)
        assert(
            is(object, "DESeqResults"),
            is(DESeqTransform, "DESeqTransform"),
            identical(rownames(object), rownames(DESeqTransform))
        )
        direction <- match.arg(direction)
        ## Rename objects internally to make the code more readable.
        res <- object
        dt <- DESeqTransform
        interestingGroups(dt) <- matchInterestingGroups(dt, interestingGroups)
        alpha <- metadata(res)[["alpha"]]
        lfcThreshold <- metadata(res)[["lfcThreshold"]]
        assert(
            isAlpha(alpha),
            isNumber(lfcThreshold),
            isNonNegative(lfcThreshold)
        )
        ## Get the character vector of DEGs.
        deg <- deg(object = res, direction = direction)
        if (length(deg) < .minDEGThreshold) {
            message(sprintf(
                fmt = "Fewer than %s DEGs to plot. Skipping.",
                .minDEGThreshold
            ))
            return(invisible())
        }
        ## Subset to only include the DEGs.
        dt <- dt[deg, , drop = FALSE]
        ## Titles.
        title <- contrastName(res)
        subtitle <- paste0(
            length(deg), " genes", ";  ",
            "alpha: ", alpha, ";  ",
            "lfcThreshold: ", lfcThreshold
        )
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



## Updated 2019-11-08.
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
        assert(isSubset(
            x = setdiff(names(call), ""),
            y = names(formals())
        ))
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
