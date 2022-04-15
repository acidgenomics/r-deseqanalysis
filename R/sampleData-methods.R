#' @name sampleData
#' @inherit AcidGenerics::sampleData
#' @note Updated 2021-03-15.
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



## Updated 2021-03-15.
`sampleData,DESeqAnalysis` <- # nolint
    function(object) {
        dds <- as(object, "DESeqDataSet")
        sampleData(dds)
    }



#' @rdname sampleData
#' @export
setMethod(
    f = "sampleData",
    signature = signature(object = "DESeqAnalysis"),
    definition = `sampleData,DESeqAnalysis`
)
