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



#' @importFrom methods show
#' @aliases NULL
#' @export
methods::show



show.DESeqAnalysis <-  # nolint
    function(object) {
        validObject(object)
        data <- slot(object, "data")
        transform <- slot(object, "transform")

        cat(paste0(
            class(object), " ", metadata(object)[["version"]], "; ",
            "DESeq2 ", metadata(data)[["version"]]
        ), sep = "\n")

        contrastNames <- .contrastNames(object)
        showSlotInfo(list(
            transform = .transformType(transform),
            contrastNames = contrastNames
        ))

        # Show information about the DESeqDataSet.
        dataInfo <- capture.output(show(data))[-1L]
        dataInfo <- paste0("  ", dataInfo)
        cat("dataSet:", dataInfo, sep = "\n")
    }



#' @rdname show
#' @export
setMethod(
    f = "show",
    signature = signature("DESeqAnalysis"),
    definition = show.DESeqAnalysis
)
