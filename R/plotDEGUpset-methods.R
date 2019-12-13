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







## Updated 2019-12-13.
`plotDEGUpset,DESeqAnalysis` <-  # nolint
    function(
        object,
        alpha = NULL,
        lfcThreshold = NULL,
        direction = c("both", "up", "down")
    ) {
        direction <- match.arg(direction)
        degPerContrast <- .degPerContrast(
            object = object,
            alpha = alpha,
            lfcThreshold = lfcThreshold,
            direction = direction
        )
        ## This will collapse the nested lists into a single flat list.
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
