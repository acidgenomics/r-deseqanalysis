#' @name degPerContrast
#' @inherit acidgenerics::baseMeanThreshold
#' @note Updated 2020-08-18.
#'
#' @inheritParams acidroxygen::params
#' @param i `character`, `numeric`, or `NULL`.
#'   Names or range of results to include in plot.
#'   If set `NULL`, include all results.
#' @param ... Passthrough arguments to [deg()].
#'
#' @return
#' - `matrix`: Matrix containing numbers of DEGs per contrast.
#'   Intended primarily for use with [plotDEGStackedBar()].
#' - `list`: Named vector containing the DEG identifiers.
#'   Intended primarily for use with [plotDEGUpset()].
#'
#' @examples
#' data(deseq)
#'
#' ## DESeqAnalysis ====
#' degPerContrast(deseq)
NULL



#' @rdname degPerContrast
#' @name degPerContrast
#' @importFrom acidgenerics degPerContrast
#' @usage degPerContrast(object, ...)
#' @export
NULL



## Updated 2020-08-12.
`degPerContrast,DESeqAnalysis` <-  # nolint
    function(
        object,
        i = NULL,
        direction = c("both", "up", "down"),
        n = FALSE,
        return = c("matrix", "list"),
        ...
    ) {
        assert(isFlag(n))
        direction <- match.arg(direction)
        return <- match.arg(return)
        if (is.null(i)) i <- resultsNames(object)
        n <- switch(EXPR = return, "matrix" = TRUE, "list" = FALSE)
        suppressMessages({
            list <- mapply(
                i = i,
                MoreArgs = list(object = object),
                FUN = function(i, object) {
                    if (isSubset(direction, c("both", "down"))) {
                        down <- deg(
                            object = object,
                            i = i,
                            direction = "down",
                            ...
                        )
                        if (isTRUE(n)) {
                            down <- length(down)
                        }
                    }
                    if (isSubset(direction, c("both", "up"))) {
                        up <- deg(
                            object = object,
                            i = i,
                            direction = "up",
                            ...
                        )
                        if (isTRUE(n)) {
                            up <- length(up)
                        }
                    }
                    if (isTRUE(n)) {
                        switch(
                            EXPR = direction,
                            "both" = c(down = down, up = up),
                            "down" = c(down = down),
                            "up" = c(up = up)
                        )
                    } else {
                        switch(
                            EXPR = direction,
                            "both" = list(down = down, up = up),
                            "down" = list(down = down),
                            "up" = list(up = up)
                        )
                    }
                },
                SIMPLIFY = FALSE,
                USE.NAMES = TRUE
            )
        })
        switch(
            EXPR = return,
            "matrix" = as.matrix(as.data.frame(list)),
            "list" = list
        )
    }



#' @rdname degPerContrast
#' @export
setMethod(
    f = "degPerContrast",
    signature = signature("DESeqAnalysis"),
    definition = `degPerContrast,DESeqAnalysis`
)
