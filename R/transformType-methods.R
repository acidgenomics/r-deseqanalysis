#' @name transformType
#' @inherit AcidGenerics::transformType
#' @note Updated 2021-03-12.
#'
#' @param ... Additional arguments.
#'
#' @seealso [DESeq2::DESeqTransform()].
#'
#' @examples
#' data(deseq)
#'
#' ## DESeqAnalysis ====
#' transformType(deseq)
NULL



## Updated 2019-07-23.
`transformType,DESeqAnalysis` <-  # nolint
    function(object) {
        validObject(object)
        dt <- as(object, "DESeqTransform")
        transformType(dt)
    }



## Updated 2019-07-23.
`transformType,DESeqAnalysisList` <-  # nolint
    function(object) {
        assert(hasLength(object))
        transformType(object[[1L]])
    }



## Updated 2021-03-12.
`transformType,DESeqTransform` <-  # nolint
    function(object) {
        validObject(object)
        ifelse(
            test = isTRUE(isSubset(
                x = "rlogIntercept",
                y = colnames(mcols(object))
            )),
            yes = "rlog",
            no = "varianceStabilizingTransformation"
        )
    }



#' @rdname transformType
#' @export
setMethod(
    f = "transformType",
    signature = signature("DESeqAnalysis"),
    definition = `transformType,DESeqAnalysis`
)



#' @rdname transformType
#' @export
setMethod(
    f = "transformType",
    signature = signature("DESeqAnalysisList"),
    definition = `transformType,DESeqAnalysisList`
)



#' @rdname transformType
#' @export
setMethod(
    f = "transformType",
    signature = signature("DESeqTransform"),
    definition = `transformType,DESeqTransform`
)
