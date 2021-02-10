#' @name sampleData
#' @inherit AcidGenerics::sampleData
#' @note Updated 2019-08-20.
#'
#' @param ... Additional arguments.
#'
#' @examples
#' data(deseq)
#'
#' ## DESeqAnalysis ====
#' x <- sampleData(deseq)
#' head(x)
NULL



## Updated 2019-08-20.
`sampleData,DESeqAnalysis` <-  # nolint
    function(object) {
        object <- as(object, "DESeqDataSet")
        sampleData(object)
    }



#' @rdname sampleData
#' @export
setMethod(
    f = "sampleData",
    signature = signature("DESeqAnalysis"),
    definition = `sampleData,DESeqAnalysis`
)
