#' @name degPerContrast
#' @inherit AcidGenerics::baseMeanThreshold
#' @note Updated 2022-05-18.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @param i `character`, `numeric`, or `NULL`.
#' Names or range of results.
#' If set `NULL`, include all results.
#'
#' @return
#' - `matrix`: Matrix containing numbers of DEGs per contrast.
#' Intended primarily for use with `plotDegStackedBar()`.
#' - `list`: Named vector containing the DEG identifiers.
#' Intended primarily for use with `plotDegUpset()`.
#'
#' @examples
#' data(deseq)
#'
#' ## DESeqAnalysis ====
#' degPerContrast(deseq)
NULL



## Updated 2022-05-17.
`degPerContrast,DESeqAnalysis` <- # nolint
    function(object,
             i = NULL,
             direction = c("both", "up", "down"),
             return = c("matrix", "list")) {
        direction <- match.arg(direction)
        return <- match.arg(return)
        n <- switch(EXPR = return,
            "matrix" = TRUE,
            "list" = FALSE
        )
        resultsNames <- resultsNames(object)
        if (is.null(i)) {
            i <- resultsNames
        } else if (is.numeric(i)) {
            i <- resultsNames[i]
        }
        assert(isSubset(i, resultsNames))
        list <- Map(
            i = i,
            MoreArgs = list("object" = object),
            f = function(i, object) {
                if (isSubset(direction, c("both", "down"))) {
                    down <- deg(
                        object = object,
                        i = i,
                        direction = "down",
                        quiet = TRUE
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
                        quiet = TRUE
                    )
                    if (isTRUE(n)) {
                        up <- length(up)
                    }
                }
                if (isTRUE(n)) {
                    switch(
                        EXPR = direction,
                        "both" = c("down" = down, "up" = up),
                        "down" = c("down" = down),
                        "up" = c("up" = up)
                    )
                } else {
                    switch(
                        EXPR = direction,
                        "both" = list("down" = down, "up" = up),
                        "down" = list("down" = down),
                        "up" = list("up" = up)
                    )
                }
            }
        )
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
    signature = signature(object = "DESeqAnalysis"),
    definition = `degPerContrast,DESeqAnalysis`
)
