#' @name degIntersection
#' @inherit AcidGenerics::degIntersection
#' @note Updated 2020-10-28.
#'
#' @inheritParams AcidRoxygen::params
#' @param i `character`, `numeric`, or `NULL`.
#'   Names or range of results.
#'   If set `NULL`, include all results.
#' @param direction `character(1)`.
#'   Include "up" or "down" directions.
#'   Must be directional, and intentionally does not support "both".
#' @param return `character(1)`.
#'   - `"matrix"`: `logical matrix`;
#'     Intersection matrix.
#'   - `"count"`: `integer`;
#'     Number of times the gene is significant across contrasts.
#'   - `"ratio"`: `numeric`;
#'     The ratio of how many times the gene is significant across contrasts.
#'   - `"names"`: `character`;
#'     Names of genes that intersect across all contrasts defined.
#'     Input of specific contrasts with `i` argument is recommended here.
#' @param ... Passthrough arguments to [deg()].
#'
#' @examples
#' data(deseq)
#'
#' ## DESeqAnalysis ====
#' mat <- degIntersection(deseq, return = "matrix")
#' class(mat)
#' type(mat)
#' head(mat)
#'
#' count <- degIntersection(deseq, return = "count")
#' class(count)
#' head(count)
#'
#' ratio <- degIntersection(deseq, return = "ratio")
#' class(ratio)
#' head(ratio)
#'
#' names <- degIntersection(deseq, i = c(1L, 2L), return = "names")
#' class(names)
#' head(names)
NULL



#' @rdname degIntersection
#' @name degIntersection
#' @importFrom AcidGenerics degIntersection
#' @usage degIntersection(object, ...)
#' @export
NULL



## Updated 2020-10-28.
`degIntersection,DESeqAnalysis` <-  # nolint
    function(
        object,
        i = NULL,
        direction = c("up", "down"),
        return = c("matrix", "count", "ratio", "names")
    ) {
        direction <- match.arg(direction)
        return <- match.arg(return)
        if (is.null(i)) i <- resultsNames(object)
        suppressMessages({
            list <- lapply(
                X = i,
                FUN = deg,
                object = object,
                direction = direction
            )
        })
        names(list) <- i
        mat <- intersectionMatrix(list)
        cli_alert(sprintf(
            "Returning intersection %s of %s %s-regulated DEGs.",
            return, nrow(mat), direction
        ))
        count <- rowSums(mat)
        mode(count) <- "integer"
        out <- switch(
            EXPR = return,
            "matrix" = mat,
            "count" = count,
            "ratio" = {
                count / length(list)
            },
            "names" = {
                x <- apply(X = mat, MARGIN = 1L, FUN = all)
                x <- names(x)[x]
                x
            }
        )
        out
    }



#' @rdname degIntersection
#' @export
setMethod(
    f = "degIntersection",
    signature = signature("DESeqAnalysis"),
    definition = `degIntersection,DESeqAnalysis`
)
