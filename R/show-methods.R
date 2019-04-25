#' Show an object
#'
#' @name show
#' @inherit methods::show
#' @examples
#' data(deseq)
#'
#' ## DESeqAnalysis ====
#' show(deseq)
NULL



#' @rdname show
#' @name show
#' @importFrom methods show
#' @export
NULL



show.DESeqAnalysis <-  # nolint
    function(object) {
        validObject(object)

        data <- slot(object, "data")
        transform <- slot(object, "transform")
        results <- slot(object, "results")

        cat(paste0(
            class(object), " ", metadata(object)[["version"]], "; ",
            "DESeq2 ", metadata(data)[["version"]]
        ), sep = "\n")

        # Show information about the DESeqDataSet.
        dataInfo <- capture.output(show(data))[-1L]
        dataInfo <- paste0("  ", dataInfo)
        cat("data:", dataInfo, sep = "\n")

        showSlotInfo(list(
            transform = transformType(transform),
            results = names(results)
        ))

        # Show information about lfcShrink method, if slotted.
        showSlotInfo(list(
            lfcShrink = lfcShrinkType(object)
        ))
    }



#' @rdname show
#' @export
setMethod(
    f = "show",
    signature = signature("DESeqAnalysis"),
    definition = show.DESeqAnalysis
)
