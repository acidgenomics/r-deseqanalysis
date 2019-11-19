## Rethink this approach, using `enriched()?`? Consider referring to method
## support in pfgsea, for example.



#' @name plotDEGUpset
#' @inherit bioverbs::plotDEGUpset
#' @note Updated 2019-11-19.
#'
#' @inheritParams acidroxygen::params
#' @inheritParams params
#' @param ... Additional arguments.
#'
#' @examples
#' data(deseq)
#' plotDEGUpset(deseq)
NULL



#' @rdname plotDEGUpset
#' @name plotDEGUpset
#' @importFrom bioverbs plotDEGUpset
#' @usage plotDEGUpset(object, ...)
#' @export
NULL



## Updated 2019-11-19.
`plotDEGUpset,DESeqAnalysis` <-  # nolint
    function(object) {
        suppressMessages(
            degPerContrast <- mapply(
                i = resultsNames(object),
                MoreArgs = list(object = object),
                FUN = function(i, object) {
                    down <- deg(
                        object = object,
                        i = i,
                        direction = "down"
                    )
                    up <- deg(
                        object = object,
                        i = i,
                        direction = "up"
                    )
                    list(down = down, up = up)
                },
                SIMPLIFY = FALSE,
                USE.NAMES = TRUE
            )
        )
        listInput <- do.call(what = c, args = degPerContrast)
        ## Using "_" instead of "." for name concatenation.
        names(listInput) <- makeNames(names(listInput), unique = TRUE)
        ## Suppressing message about contrast not having up/down DEG overlap:
        ## geom_path: Each group consists of only one observation.
        suppressMessages(
            plotUpset(object = fromList(listInput))
        )
    }



#' @rdname plotDEGUpset
#' @export
setMethod(
    f = "plotDEGUpset",
    signature = signature("DESeqAnalysis"),
    definition = `plotDEGUpset,DESeqAnalysis`
)
