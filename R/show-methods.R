#' Show an object
#'
#' @name show
#' @inherit methods::show params return title
#' @note Updated 2019-11-13.
#'
#' @examples
#' data(deseq)
#'
#' ## DESeqAnalysis ====
#' show(deseq)
NULL



## Updated 2019-07-23.
`show,DESeqAnalysis` <-  # nolint
    function(object) {
        validObject(object)
        dds <- as(object, "DESeqDataSet")

        cat(paste0(
            class(object), " ", metadata(object)[["version"]], "; ",
            "DESeq2 ", metadata(dds)[["version"]]
        ), sep = "\n")

        ## Show information about the DESeqDataSet.
        ddsInfo <- paste0("  ", capture.output(show(dds))[-1L])
        cat("data:", ddsInfo, sep = "\n")


        res <- as(object, "DESeqResults")
        alpha <- metadata(res)[["alpha"]]
        lfcThreshold <- metadata(res)[["lfcThreshold"]]

        showSlotInfo(list(
            transformType = transformType(object),
            resultsNames = resultsNames(object),
            alpha = alpha,
            lfcThreshold = lfcThreshold,
            lfcShrinkType = lfcShrinkType(object)
        ))
    }



#' @rdname show
#' @export
setMethod(
    f = "show",
    signature = signature("DESeqAnalysis"),
    definition = `show,DESeqAnalysis`
)
