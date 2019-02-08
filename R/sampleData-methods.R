#' @name sampleData
#' @export
#' @importFrom bioverbs sampleData
#' @inherit bioverbs::sampleData
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
