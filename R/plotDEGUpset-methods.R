#' @name plotDEGUpset
#' @inherit acidgenerics::plotDEGUpset
#' @note Updated 2020-08-25.
#'
#' @inheritParams degPerContrast
#' @inheritParams acidroxygen::params
#' @inheritParams params
#' @param ... Passthrough arguments to [acidplots::plotUpset()].
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



## Updated 2020-08-25.
`plotDEGUpset,DESeqAnalysis` <-  # nolint
    function(
        object,
        i = NULL,
        direction = c("both", "up", "down"),
        ...
    ) {
        direction <- match.arg(direction)
        degPerContrast <- degPerContrast(
            object = object,
            i = i,
            direction = direction,
            return = "list"
        )
        list <- do.call(what = c, args = degPerContrast)
        names(list) <- makeNames(names(list), unique = TRUE)
        plotUpset(list, ...)
    }



#' @rdname plotDEGUpset
#' @export
setMethod(
    f = "plotDEGUpset",
    signature = signature("DESeqAnalysis"),
    definition = `plotDEGUpset,DESeqAnalysis`
)
