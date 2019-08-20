#' @name sampleData
#' @inherit bioverbs::sampleData
#' @note Updated 2019-08-20.
#'
#' @param ... Additional arguments.
#'
#' @examples
#' data(deseq)
#' x <- sampleData(deseq)
#' head(x)
NULL



#' @rdname sampleData
#' @name sampleData
#' @importFrom bioverbs sampleData
#' @usage sampleData(object, ...)
#' @export
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
