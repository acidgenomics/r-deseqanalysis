#' Show an object
#'
#' @name show
#' @inherit methods::show params return title
#' @note Updated 2020-08-04.
#'
#' @examples
#' data(deseq)
#'
#' ## DESeqAnalysis ====
#' show(deseq)
NULL



## Updated 2020-08-04.
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
        list <- list(
            transformType = transformType(object),
            resultsNames = resultsNames(object),
            alphaThreshold = alphaThreshold(object)
        )
        lfcShrink <- lfcShrink(object)
        if (isTRUE(lfcShrink)) {
            list[["lfcShrinkType"]] <- lfcShrinkType(object)
        }
        lfcThreshold <- lfcThreshold(object)
        if (lfcThreshold > 0L) {
            list[["lfcThreshold"]] <- lfcThreshold
        }
        baseMeanThreshold = baseMeanThreshold(object)
        if (baseMeanThreshold > 0L) {
            list[["baseMeanThreshold"]] <- baseMeanThreshold
        }
        showSlotInfo(list)
    }



#' @rdname show
#' @export
setMethod(
    f = "show",
    signature = signature("DESeqAnalysis"),
    definition = `show,DESeqAnalysis`
)
