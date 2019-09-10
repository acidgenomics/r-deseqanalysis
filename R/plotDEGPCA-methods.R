## Do not allow post hoc alpha or lfcThreshold cutoffs here.



#' @name plotDEGPCA
#' @inherit bioverbs::plotDEGPCA
#' @note Updated 2019-09-10.
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
#' plotDEGPCA(deseq, results = 1L)
NULL



#' @rdname plotDEGPCA
#' @name plotDEGPCA
#' @importFrom bioverbs plotDEGPCA
#' @usage plotDEGPCA(object, ...)
#' @export
NULL



## Updated 2019-07-23.
`plotDEGPCA,DESeqResults` <-  # nolint
    function(
        object,
        DESeqTransform,  # nolint
        direction = c("both", "up", "down")
    ) {
        validObject(object)
        validObject(DESeqTransform)
        assert(
            is(object, "DESeqResults"),
            is(DESeqTransform, "DESeqTransform"),
            identical(rownames(object), rownames(DESeqTransform))
        )
        direction <- match.arg(direction)
        return <- match.arg(return)
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
        if (!hasLength(deg)) {
            warning("There are no DEGs to plot. Skipping.")
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
        rse <- as(dt, "RangedSummarizedExperiment")
        do.call(
            what = plotPCA,
            args = list(
                object = rse,
                interestingGroups = interestingGroups,
                ## We're using our DEGs instead of top (500) variable genes.
                ntop = Inf,
                label = label,
                title = title,
                subtitle = subtitle,
                return = return
            )
        )
    }

f1 <- formals(`plotDEGPCA,DESeqResults`)
f2 <- methodFormals(
    f = "plotPCA",
    signature = "SummarizedExperiment",
    package = "acidplots"
)
f2 <- f2[setdiff(names(f2), c("ntop", "subtitle", "title"))]
f <- c(f1, f2)
formals(`plotDEGPCA,DESeqResults`) <- f



#' @rdname plotDEGPCA
#' @export
setMethod(
    f = "plotDEGPCA",
    signature = signature("DESeqResults"),
    definition = `plotDEGPCA,DESeqResults`
)



## Updated 2019-09-09.
`plotDEGPCA,DESeqAnalysis` <-  # nolint
    function(
        object,
        results,
        contrastSamples = FALSE
    ) {
        validObject(object)
        assert(
            isScalar(results),
            isFlag(contrastSamples)
        )
        ## Note that LFC values aren't used for this plot, just the DEGs, which
        ## are used to subset the DESeqTransform counts.
        res <- results(object, results = results, lfcShrink = FALSE)
        if (!isString(results)) {
            name <- contrastNames(object)[[results]]
        } else {
            name <- results
        }
        contrastName(res) <- name
        validObject(res)
        ## Using the variance-stabilized counts for visualization.
        dt <- as(object, "DESeqTransform")
        validObject(dt)
        ## Subset the DESeqTransform, if necessary.
        if (isTRUE(contrastSamples)) {
            samples <- contrastSamples(object, results = results)
            assert(isSubset(samples, colnames(dt)))
            dt <- dt[, samples, drop = FALSE]
            dt <- droplevels(dt)
        }
        ## Passing through to DESeqResults/DESeqTransform method here.
        do.call(
            what = plotDEGPCA,
            args = matchArgsToDoCall(
                args = list(
                    object = res,
                    DESeqTransform = dt
                ),
                removeFormals = c(
                    "results",
                    "contrastSamples"
                )
            )
        )
    }

f1 <- formals(`plotDEGPCA,DESeqAnalysis`)
f2 <- formals(`plotDEGPCA,DESeqResults`)
f2 <- f2[setdiff(names(f2), c(names(f1), "DESeqTransform"))]
f <- c(f1, f2)
formals(`plotDEGPCA,DESeqAnalysis`) <- f



#' @rdname plotDEGPCA
#' @export
setMethod(
    f = "plotDEGPCA",
    signature = signature("DESeqAnalysis"),
    definition = `plotDEGPCA,DESeqAnalysis`
)
