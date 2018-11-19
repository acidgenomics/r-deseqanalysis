# TODO Show lfcThreshold info on the plot.
# FIXME Consider not exporting DESeqResults here...
# Do not allow post hoc alpha, lfcThreshold cutoffs.



#' @name plotDEGPCA
#' @inherit basejump::plotDEGPCA
#' @inheritParams basejump::plotPCA
#' @inheritParams basejump::params
#' @inheritParams params
#' 
#' @param counts `DESeqTransform`.
#'
#' @examples
#' data(deseq)
#'
#' ## DESeqAnalysis (recommended) ====
#' plotDEGPCA(deseq)
#'
#' ## DESeqResults (legacy; not recommended) ====
#' plotDEGPCA(
#'     object = as(deseq, "DESeqResults"),
#'     counts = as(deseq, "DESeqTransform")
#' )
NULL



#' @importFrom basejump plotDEGPCA
#' @aliases NULL
#' @export
basejump::plotDEGPCA



plotDEGPCA.DESeqResults <-  # nolint
    function(
        object,
        counts,
        direction = c("both", "up", "down")
    ) {
        assert_is_all_of(object, "DESeqResults")
        validObject(object)
        assert_is_all_of(counts, "DESeqTransform")
        validObject(counts)
        assert_are_identical(rownames(object), rownames(counts))
        interestingGroups <- matchInterestingGroups(
            object = counts,
            interestingGroups = interestingGroups
        )
        interestingGroups(counts) <- interestingGroups
        alpha <- metadata(object)[["alpha"]]
        assertIsAlpha(alpha)
        lfcThreshold <- metadata(object)[["lfcThreshold"]]
        assert_is_a_number(lfcThreshold)
        assert_all_are_non_negative(lfcThreshold)
        direction <- match.arg(direction)
        return <- match.arg(return)

        # Get the character vector of DEGs.
        deg <- .deg(
            object = object,
            alpha = alpha,
            lfcThreshold = lfcThreshold,
            direction = direction
        )
        if (!has_length(deg)) {
            warning("No significant DEGs to plot.", call. = FALSE)
            return(invisible())
        }

        # Using SummarizedExperiment method here.
        se <- as(counts[deg, , drop = FALSE], "SummarizedExperiment")
        do.call(
            what = plotPCA,
            args = list(
                object = se,
                interestingGroups = interestingGroups,
                ntop = Inf,
                label = label,
                title = contrastName(object),
                subtitle = paste(length(deg), "genes;", "alpha <", alpha),
                return = return
            )
        )
    }

f1 <- formals(plotDEGPCA.DESeqResults)
f2 <- methodFormals(
    f = "plotPCA", 
    signature = "SummarizedExperiment",
    package = "basejump"
)
f2 <- f2[setdiff(names(f2), c("ntop", "subtitle", "title"))]
f <- c(f1, f2)
formals(plotDEGPCA.DESeqResults) <- f



#' @rdname plotDEGPCA
#' @export
setMethod(
    f = "plotDEGPCA",
    signature = signature("DESeqResults"),
    definition = plotDEGPCA.DESeqResults
)



plotDEGPCA.DESeqAnalysis <-  # nolint
    function(object, results) {
        results <- .matchResults(object = object, results = results)
        counts <- slot(object, "transform")
        do.call(
            what = plotDEGPCA,
            args = matchArgsToDoCall(
                args = list(object = results, counts = counts),
                removeFormals = "results"
            )
        )
    }

f1 <- formals(plotDEGPCA.DESeqAnalysis)
f2 <- formals(plotDEGPCA.DESeqResults)
f2 <- f2[setdiff(names(f2), names(f1))]
f <- c(f1, f2)
formals(plotDEGPCA.DESeqAnalysis) <- f



#' @rdname plotDEGPCA
#' @export
setMethod(
    f = "plotDEGPCA",
    signature = signature("DESeqAnalysis"),
    definition = plotDEGPCA.DESeqAnalysis
)
