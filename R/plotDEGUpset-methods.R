#' UpSet plot of directional DEG intersections across contrasts
#'
#' @name plotDEGUpset
#'
#' @inheritParams params
#' @param ... Additional arguments.
#'
#' @examples
#' data(deseq)
#' plotDEGUpset(deseq)
NULL



plotDEGUpset.DESeqAnalysis <- function(object) {
    degPerContrast <- mapply(
        results = resultsNames(object),
        MoreArgs = list(object = object),
        FUN = function(results, object) {
            down <- deg(object = object, results = results, direction = "down")
            up <- deg(object = object, results = results, direction = "up")
            list(down = down, up = up)
        },
        SIMPLIFY = FALSE,
        USE.NAMES = TRUE
    )
    listInput <- do.call(what = c, args = degPerContrast)
    # Use "_" instead of "." for name concatenation.
    names(listInput) %<>% makeNames(unique = TRUE)
    upset(data = fromList(listInput))
}



#' @rdname plotDEGUpset
#' @export
setMethod(
    f = "plotDEGUpset",
    signature = signature("DESeqAnalysis"),
    definition = plotDEGUpset.DESeqAnalysis
)
