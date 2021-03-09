## FIXME NEED TO RETHINK THIS WITH DESEQANALYSISLIST SUPPORT.



#' @name transformType
#' @inherit AcidGenerics::transformType
#' @note Updated 2020-08-04.
#' @param ... Additional arguments.
#' @seealso [DESeq2::DESeqTransform()].
#' @examples
#' data(deseq)
#'
#' ## DESeqAnalysis ====
#' transformType(deseq)
NULL



## Updated 2019-07-23.
`transformType,DESeqTransform` <-  # nolint
    function(object) {
        validObject(object)
        if ("rlogIntercept" %in% colnames(mcols(object))) {
            "rlog"
        } else {
            "varianceStabilizingTransformation"
        }
    }



#' @rdname transformType
#' @export
setMethod(
    f = "transformType",
    signature = signature("DESeqTransform"),
    definition = `transformType,DESeqTransform`
)



## Updated 2019-07-23.
`transformType,DESeqAnalysis` <-  # nolint
    function(object) {
        validObject(object)
        dt <- as(object, "DESeqTransform")
        transformType(dt)
    }



#' @rdname transformType
#' @export
setMethod(
    f = "transformType",
    signature = signature("DESeqAnalysis"),
    definition = `transformType,DESeqAnalysis`
)
