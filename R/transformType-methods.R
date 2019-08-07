#' Variance-stabilizing transformation type
#'
#' @name transformType
#' @inheritParams acidroxygen::params
#'
#' @seealso [DESeq2::DESeqTransform()].
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
