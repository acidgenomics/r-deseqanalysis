#' Show an object
#'
#' @name show
#' @inherit methods::show params return title
#' @note Updated 2021-06-29.
#'
#' @examples
#' data(deseq)
#'
#' ## DESeqAnalysis ====
#' show(deseq)
NULL



## Updated 2021-10-15.
`show,DESeqAnalysis` <- # nolint
    function(object) {
        validObject(object)
        dds <- as(object, "DESeqDataSet")
        cat(paste0(
            class(object), " ",
            metadata(object)[["packageVersion"]], "; ",
            "DESeq2 ", metadata(dds)[["version"]]
        ), sep = "\n")
        ## Show information about the DESeqDataSet.
        ddsInfo <- paste0("  ", capture.output(show(dds))[-1L])
        cat("data:", ddsInfo, sep = "\n")
        list <- list(
            "transformType" = transformType(object),
            "resultsNames" = resultsNames(object),
            "alphaThreshold" = alphaThreshold(object)
        )
        baseMeanThreshold <- baseMeanThreshold(object)
        if (baseMeanThreshold > 0L) {
            list[["baseMeanThreshold"]] <- baseMeanThreshold
        }
        lfcThreshold <- lfcThreshold(object)
        if (lfcThreshold > 0L) {
            if (lfcThreshold(results(object, i = 1L)) == 0L) {
                lfcThreshold <- paste(lfcThreshold, "(post-hoc)")
            }
            list[["lfcThreshold"]] <- lfcThreshold
        }
        lfcShrink <- lfcShrink(object)
        if (isTRUE(lfcShrink)) {
            list[["lfcShrinkType"]] <- lfcShrinkType(object)
        }
        showSlotInfo(list)
    }



#' @rdname show
#' @export
setMethod(
    f = "show",
    signature = signature(object = "DESeqAnalysis"),
    definition = `show,DESeqAnalysis`
)
