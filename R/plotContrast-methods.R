#' Plot differential expression contrast
#'
#' @name plotContrast
#'
#' @note Updated 2021-08-02.
#'
#' @examples
#' data(deseq)
#' plotContrast(deseq, i = 1L)
#'
#' @seealso
# - https://doi.org/10.1084/jem.20200829
NULL



## FIXME Consider adding a "geom" argument here, for different types of plots.

## Updated 2021-08-02.
`plotContrast,DESeqAnalysis` <-
    function(object, i) {
    }



#' @rdname plotContrast
#' @export
setMethod(
    f = "plotContrast",
    signature = signature("DESeqAnalysis"),
    definition = `plotContrast,DESeqAnalysis`
)
