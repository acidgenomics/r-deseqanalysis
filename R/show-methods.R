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



.showHeader <- function(object, version = NULL) {
    cat(paste(class(object), version), sep = "\n")
}



show.DESeqAnalysis <-  # nolint
    function(object) {
        validObject(object)
        data <- slot(object, "data")
        transform <- slot(object, "transform")
        .showHeader(
            object = object,
            version = metadata(data)[["version"]]
        )
        contrastNames <- .contrastNames(object)
        showSlotInfo(list(
            transform = .transformType(transform),
            contrastNames = contrastNames
        ))
        cat(capture.output(show(data)), sep = "\n")
    }



#' @rdname show
#' @export
setMethod(
    f = "show",
    signature = signature("DESeqAnalysis"),
    definition = show.DESeqAnalysis
)
