#' @name plotDEGUpset
#' @inherit AcidGenerics::plotDEGUpset
#' @note Updated 2022-05-18.
#'
#' @inheritParams degPerContrast
#' @inheritParams AcidRoxygen::params
#' @inheritParams params
#' @param ... Passthrough arguments to `AcidPlots::plotUpset()`.
#'
#' @examples
#' data(deseq)
#'
#' ## DESeqAnalysis ====
#' plotDEGUpset(deseq)
NULL



## Updated 2022-05-18.
`plotDEGUpset,DESeqAnalysis` <- # nolint
    function(object,
             i = NULL,
             direction = c("both", "up", "down"),
             ...) {
        direction <- match.arg(direction)
        degs <- degPerContrast(
            object = object,
            i = i,
            direction = direction,
            return = "list"
        )
        list <- do.call(what = c, args = degs)
        names(list) <- makeNames(names(list), unique = TRUE)
        if (!any(bapply(X = list, FUN = hasLength))) {
            alertWarning("No DEGs to plot. Skipping.")
            return(invisible(NULL))
        }
        plotUpset(object = list, ...)
    }



#' @rdname plotDEGUpset
#' @export
setMethod(
    f = "plotDEGUpset",
    signature = signature(object = "DESeqAnalysis"),
    definition = `plotDEGUpset,DESeqAnalysis`
)
