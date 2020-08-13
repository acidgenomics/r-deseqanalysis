#' @name plotDEGUpset
#' @inherit acidgenerics::plotDEGUpset
#' @note Updated 2020-08-12.
#'
#' @inheritParams degPerContrast
#' @inheritParams acidroxygen::params
#' @inheritParams params
#' @param ... Additional arguments.
#'
#' @examples
#' data(deseq)
#'
#' ## DESeqAnalysis ====
#' plotDEGUpset(deseq)
NULL



#' @rdname plotDEGUpset
#' @name plotDEGUpset
#' @importFrom acidgenerics plotDEGUpset
#' @usage plotDEGUpset(object, ...)
#' @export
NULL



## Updated 2020-08-12.
`plotDEGUpset,DESeqAnalysis` <-  # nolint
    function(
        object,
        i = NULL,
        direction = c("both", "up", "down")
    ) {
        direction <- match.arg(direction)
        degPerContrast <- degPerContrast(
            object = object,
            i = i,
            direction = direction,
            return = "list"
        )
        listInput <- do.call(what = c, args = degPerContrast)
        names(listInput) <- makeNames(names(listInput), unique = TRUE)
        suppressMessages({
            plotUpset(listInput)
        })
    }



#' @rdname plotDEGUpset
#' @export
setMethod(
    f = "plotDEGUpset",
    signature = signature("DESeqAnalysis"),
    definition = `plotDEGUpset,DESeqAnalysis`
)
