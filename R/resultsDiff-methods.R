#' Relative difference of DESeq results
#'
#' @name resultsDiff
#' @note Updated 2019-12-17.
#'
#' @inheritParams params
NULL



## Updated 2019-12-17.
`resultsDiff,DESeqResults,DESeqResults` <-
    function(x, y, col = c("log2FoldChange", "stat")) {
        stop("NOT DEFINED YET")
    }



#' @rdname resultsDiff
#' @export
setMethod(
    f = "resultsDiff",
    signature = signature(
        x = "DESeqResults",
        y = "DESeqResults"
    ),
    definition = `resultsDiff,DESeqResults,DESeqResults`
)



## Updated 2019-12-17.
`resultsDiff,DESeqAnalysis,missing` <-
    function(x, y = NULL, i, j, col) {
        stop("NOT DEFINED YET")
    }

formals(`resultsDiff,DESeqAnalysis,missing`)[["col"]] <-
    formals(`resultsDiff,DESeqResults,DESeqResults`)[["col"]]



#' @rdname resultsDiff
#' @export
setMethod(
    f = "resultsDiff",
    signature = signature(
        x = "DESeqAnalysis",
        y = "missingOrNULL"
    ),
    definition = `resultsDiff,DESeqAnalysis,missing`
)
