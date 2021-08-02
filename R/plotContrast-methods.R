## FIXME Need to support plotVolcano colors here...
## FIXME Consider supporting directionality labeling.



#' Plot differential expression contrast
#'
#' @name plotContrast
#' @note Updated 2021-08-02.
#'
#' @inheritParams params
#'
#' @examples
#' data(deseq)
#' plotContrast(deseq, i = 1L)
#'
#' @seealso
# - https://doi.org/10.1084/jem.20200829
NULL



## FIXME Consider adding a "geom" argument here, for different types of plots.
## FIXME How to return the contrast groups here...Need a function.

## Updated 2021-08-02.
`plotContrast,DESeqAnalysis` <-
    function(object, i) {
        validObject(object)
        res <- results(object, i = i)
        ## FIXME This needs to stash more metadata...
        ## FIXME Needs to return "group", "numerator" name, "denominator" name.
        samplesList <- contrastSamples(
            object = object,
            i = i,
            quiet = TRUE,
            return = "list"
        )
        assert(
            is.list(samplesList),
            identical(names(samplesList), c("numerator", "denominator"))
        )
        samples <- c(samplesList[["numerator"]], samplesList[["denominator"]])
        dds <- as(object, "DESeqDataSet")
        dds <- dds[, samples, drop = FALSE]
        counts <- counts(dds, normalized = TRUE)
        ## Only include non-zero counts on the plot.
        keep <- rowSums(counts) > 0L
        counts <- counts[keep, , drop = FALSE]
        ## Take the mean of the non-zero counts, per contrast.
        logcounts <- log2(counts + 1L)
        ## FIXME Need to label DEGs here....
        ## FIXME Use "isDEG" column here (see plotMA code).
        data <- data.frame(
            "x" = rowMeans(logcounts[, samplesList[["denominator"]]]),
            "y" = rowMeans(logcounts[, samplesList[["numerator"]]]),
            row.names = rownames(counts)
        )
        p <- ggplot(
            data = data,
            mapping = aes(
                x = !!sym("x"),
                y = !!sym("y")
                ## fill = !!sym("isDEG")
            )
        ) +
            geom_point(size = 1L)



        stop("FIXME Incomplete")
    }



#' @rdname plotContrast
#' @export
setMethod(
    f = "plotContrast",
    signature = signature("DESeqAnalysis"),
    definition = `plotContrast,DESeqAnalysis`
)
