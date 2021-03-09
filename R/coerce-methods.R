#' Force an object to belong to a class
#'
#' @name as
#' @aliases coerce
#' @author Michael Steinbaugh
#' @exportMethod coerce
#' @importFrom methods coerce
#' @inherit methods::as return title
#' @note Updated 2021-03-09.
#'
#' @section DESeqAnalysis:
#'
#' Supported coercion methods will extract any of these internal objects:
#'
#' - `DESeqDataSet`.
#' - `DESeqTransform`.
#' - `DESeqResultsList`.
#'
#' @examples
#' data(deseq)
#'
#' ## DESeqAnalysis ====
#' dds <- as(deseq, "DESeqDataSet")
#' print(dds)
#' dt <- as(deseq, "DESeqTransform")
#' print(dt)
#' resList <- as(deseq, "DESeqResultsList")
#' print(names(resList))
NULL



#' @rdname as
#' @name coerce,DESeqAnalysis,DESeqDataSet-method
## Updated 2019-07-23.
setAs(
    from = "DESeqAnalysis",
    to = "DESeqDataSet",
    function(from) {
        validObject(from)
        to <- slot(from, "data")
        validObject(to)
        to
    }
)



#' @rdname as
#' @name coerce,DESeqAnalysis,DESeqTransform-method
## Updated 2019-07-23.
setAs(
    from = "DESeqAnalysis",
    to = "DESeqTransform",
    function(from) {
        validObject(from)
        to <- slot(from, "transform")
        validObject(to)
        to
    }
)



#' @rdname as
#' @name coerce,DESeqAnalysis,DESeqResultsList-method
## Updated 2021-03-09.
setAs(
    from = "DESeqAnalysis",
    to = "DESeqResultsList",
    function(from) {
        validObject(from)
        to <- DESeqResultsList(from, quiet = TRUE)
        to
    }
)
