#' Variance-stabilizing transformation type
#'
#' @name transformType
#' @inheritParams basejump::params
#'
#' @seealso [DESeq2::DESeqTransform()].
NULL



transformType.DESeqTransform <-  # nolint
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
    definition = transformType.DESeqTransform
)



transformType.DESeqAnalysis <-  # nolint
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
    definition = transformType.DESeqAnalysis
)
