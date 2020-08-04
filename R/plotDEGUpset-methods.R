#' @name plotDEGUpset
#' @inherit acidgenerics::plotDEGUpset
#' @note Updated 2020-08-04.
#'
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



## Updated 2020-08-04.
`plotDEGUpset,DESeqAnalysis` <-  # nolint
    function(
        object,
        alphaThreshold = NULL,
        lfcThreshold = NULL,
        baseMeanThreshold = NULL,
        direction = c("both", "up", "down")
    ) {
        direction <- match.arg(direction)
        degPerContrast <- degPerContrast(
            object = object,
            alphaThreshold = alphaThreshold,
            lfcThreshold = lfcThreshold,
            baseMeanThreshold = baseMeanThreshold,
            direction = direction,
            return = "list"
        )
        ## This will collapse the nested lists into a single flat list.
        listInput <- do.call(what = c, args = degPerContrast)
        ## Using "_" instead of "." for name concatenation.
        names(listInput) <- makeNames(names(listInput), unique = TRUE)
        ## Suppressing message about contrast not having up/down DEG overlap.
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
