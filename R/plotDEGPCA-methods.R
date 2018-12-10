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
        results = 1L,
        contrastSamples = FALSE,
        direction = c("both", "up", "down")
    ) {
        validObject(object)
        results <- .matchResults(object, results)
        validObject(results)
        counts <- as(object, "DESeqTransform")
        validObject(counts)
        assert_are_identical(rownames(results), rownames(counts))
        interestingGroups <- matchInterestingGroups(counts, interestingGroups)
        interestingGroups(counts) <- interestingGroups
        alpha <- metadata(results)[["alpha"]]
        assertIsAlpha(alpha)
        lfcThreshold <- metadata(results)[["lfcThreshold"]]
        assert_is_a_number(lfcThreshold)
        assert_all_are_non_negative(lfcThreshold)
        assert_is_a_bool(contrastSamples)
        direction <- match.arg(direction)
        return <- match.arg(return)

        # Get the character vector of DEGs.
        deg <- deg(object = results, direction = direction)
        if (length(deg) == 0L) {
            return(invisible())
        }

        se <- counts %>%
            as("RangedSummarizedExperiment") %>%
            as("SummarizedExperiment") %>%
            .[deg, , drop = FALSE]

        # Subset the counts to match contrast samples, if desired.
        if (isTRUE(contrastSamples)) {
            samples <- contrastSamples(object)
            assert_is_subset(samples, colnames(se))
            se <- se[, samples, drop = FALSE]
            colData(se) <- relevelColData(colData(se))
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
                object = se,
                interestingGroups = interestingGroups,
                ntop = Inf,
                label = label,
                title = contrastName(results),
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
