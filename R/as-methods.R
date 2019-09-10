#' Force an object to belong to a class
#'
#' @name as
#' @aliases coerce
#' @author Michael Steinbaugh
#' @exportMethod coerce
#' @importFrom methods coerce
#' @inherit methods::as
#' @inheritParams coerce
#' @note Updated 2019-08-20.
#'
#' @section DESeqAnalysis:
#'
#' Supported coercion methods will extract any of these internal objects:
#'
#' - `DESeqDataSet`.
#' - `DESeqTransform`.
#' - `DESeqResults`. Extracts the first results slotted. Note that this
#'   corresponds to results containing log2 fold change (LFC) values that *have
#'   not been shrunken* using [DESeq2::lfcShrink()].
#'
#' @examples
#' data(deseq)
#'
#' ## DESeqAnalysis ====
#' dds <- as(deseq, "DESeqDataSet")
#' print(dds)
#' dt <- as(deseq, "DESeqTransform")
#' print(dt)
#' ## Pulls the first results slotted.
#' res <- as(deseq, "DESeqResults")
#' summary(res)
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
#' @name coerce,DESeqAnalysis,DESeqResults-method
## Updated 2019-07-23.
setAs(
    from = "DESeqAnalysis",
    to = "DESeqResults",
    function(from) {
        validObject(from)
        to <- slot(from, "results")[[1L]]
        validObject(to)
        to
    }
)
