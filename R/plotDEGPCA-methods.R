# Do not allow post hoc alpha or lfcThreshold cutoffs here.



#' @name plotDEGPCA
#' @inherit basejump::plotDEGPCA
#' @inheritParams basejump::plotPCA
#' @inheritParams basejump::params
#' @inheritParams params
#'
#' @examples
#' data(deseq)
#'
#' ## DESeqAnalysis ====
#' plotDEGPCA(deseq)
NULL



#' @importFrom basejump plotDEGPCA
#' @aliases NULL
#' @export
basejump::plotDEGPCA



plotDEGPCA.DESeqAnalysis <-  # nolint
    function(
        object,
        results,
        contrastSamples = FALSE,
        direction = c("both", "up", "down")
    ) {
        validObject(object)
        assert(isFlag(contrastSamples))
        direction <- match.arg(direction)
        return <- match.arg(return)

        res <- .matchResults(object, results)
        validObject(res)

        # Using the variance-stabilized counts for visualization.
        dt <- as(object, "DESeqTransform")
        validObject(dt)

        assert(identical(rownames(res), rownames(dt)))

        interestingGroups(dt) <-
            matchInterestingGroups(dt, interestingGroups)

        alpha <- metadata(res)[["alpha"]]
        assert(containsAlpha(alpha))

        lfcThreshold <- metadata(res)[["lfcThreshold"]]
        assert(
            isNumber(lfcThreshold),
            isNonNegative(lfcThreshold)
        )

        # Get the character vector of DEGs.
        deg <- deg(res, direction = direction)
        if (!hasLength(deg)) {
            message("There are no DEGs to plot. Skipping.")
            return(invisible())
        }

        # Subset to only include the DEGs.
        dt <- dt[deg, , drop = FALSE]

        # Subset the counts to match contrast samples, if desired.
        if (isTRUE(contrastSamples)) {
            samples <- contrastSamples(object, results = results)
            assert(isSubset(samples, colnames(dt)))
            dt <- dt[, samples, drop = FALSE]
            colData(dt) <- relevelColData(colData(dt))
        }

        # Subtitle.
        subtitle <- paste0(length(deg), " genes; alpha < ", alpha)
        if (lfcThreshold > 0L) {
            subtitle <- paste0(subtitle, "; lfc > ", lfcThreshold)
        }

        # Using SummarizedExperiment method here.
        do.call(
            what = plotPCA,
            args = list(
                object = as(dt, "RangedSummarizedExperiment"),
                interestingGroups = interestingGroups,
                ntop = Inf,
                label = label,
                title = contrastName(res),
                subtitle = subtitle,
                return = return
            )
        )
    }

f1 <- formals(plotDEGPCA.DESeqAnalysis)
f2 <- methodFormals(
    f = "plotPCA",
    signature = "SummarizedExperiment",
    package = "basejump"
)
f2 <- f2[setdiff(names(f2), c("ntop", "subtitle", "title"))]
f <- c(f1, f2)
formals(plotDEGPCA.DESeqAnalysis) <- f



#' @rdname plotDEGPCA
#' @export
setMethod(
    f = "plotDEGPCA",
    signature = signature("DESeqAnalysis"),
    definition = plotDEGPCA.DESeqAnalysis
)
