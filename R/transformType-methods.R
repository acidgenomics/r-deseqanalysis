#' @name transformType
#' @inherit AcidGenerics::transformType
#' @note Updated 2021-06-28.
#'
#' @param ... Additional arguments.
#'
#' @seealso `DESeq2::DESeqTransform()`.
#'
#' @examples
#' data(deseq)
#'
#' ## DESeqAnalysis ====
#' transformType(deseq)
NULL



## Updated 2023-12-18.
`transformType,DESeqAnalysis` <- # nolint
    function(object) {
        dt <- as(object, "DESeqTransform")
        transformType(dt)
    }



## Updated 2019-07-23.
`transformType,DESeqAnalysisList` <- # nolint
    function(object) {
        assert(hasLength(object))
        transformType(object[[1L]])
    }



## Updated 2023-12-18.
`transformType,DESeqTransform` <- # nolint
    function(object) {
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
    signature = signature(object = "DESeqAnalysis"),
    definition = `transformType,DESeqAnalysis`
)

#' @rdname transformType
#' @export
setMethod(
    f = "transformType",
    signature = signature(object = "DESeqAnalysisList"),
    definition = `transformType,DESeqAnalysisList`
)

#' @rdname transformType
#' @export
setMethod(
    f = "transformType",
    signature = signature(object = "DESeqTransform"),
    definition = `transformType,DESeqTransform`
)
