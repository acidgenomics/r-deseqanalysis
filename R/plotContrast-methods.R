#' Plot differential expression contrast
#'
#' @name plotContrast
#'
#' @note Updated 2021-08-02.
#'
#' @examples
#' data(deseq)
#' plotContrast(deseq, i = 1L)
#'
#' @seealso
# - https://doi.org/10.1084/jem.20200829
NULL



## FIXME Consider adding a "geom" argument here, for different types of plots.

## Updated 2021-08-02.
`plotContrast,DESeqAnalysis` <-
    function(object, i) {
        validObject(object)
        res <- results(object, i = i)
        samples <- contrastSamples(
            object = object,
            i = i,
            quiet = TRUE
        )
        dds <- as(object, "DESeqDataSet")
        dds <- dds[, samples, drop = FALSE]
        counts <- counts(dds, normalized = TRUE)
        ## Only include non-zero counts on the plot.
        keep <- rowSums(counts) > 0L
        counts <- counts[keep, , drop = FALSE]

        ## FIXME Need to use updated `contrastSamples` function here
        ## to get "numerator" and "denominator" samples.
        ##
        ## FIXME Then we can use these mappings to calculate mean values
        ## per grouping.

        ## Take the mean of the non-zero counts, per contrast.
        logcounts <- log2(counts + 1L)

        stop("FIXME Incomplete")
    }



#' @rdname plotContrast
#' @export
setMethod(
    f = "plotContrast",
    signature = signature("DESeqAnalysis"),
    definition = `plotContrast,DESeqAnalysis`
)
