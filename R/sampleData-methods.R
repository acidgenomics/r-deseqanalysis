#' @name sampleData
#' @inherit bioverbs::sampleData
#' @param ... Additional arguments.
#'
#' @examples
#' data(deseq)
#' x <- sampleData(deseq)
#' head(x)
NULL



sampleData.DESeqAnalysis <-  # nolint
    function(object) {
        object %>%
            as("DESeqDataSet") %>%
            sampleData()
    }



#' @rdname sampleData
#' @export
setMethod(
    f = "sampleData",
    signature = signature("DESeqAnalysis"),
    definition = sampleData.DESeqAnalysis
)
