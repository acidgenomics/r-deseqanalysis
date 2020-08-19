#' @name degIntersection
#' @inherit acidgenerics::degIntersection
#' @note Updated 2020-08-18.
#'
#' @inheritParams acidroxygen::params
#' @param i `character`, `numeric`, or `NULL`.
#'   Names or range of results.
#'   If set `NULL`, include all results.
#' @param direction `character(1)`.
#'   Include "up" or "down" directions.
#'   Must be directional, and intentionally does not support "both".
#' @param return `character(1)`.
#'   Return intersection matrix, count per contrast, or ratio.
#' @param ... Passthrough arguments to [deg()].
#'
#' @examples
#' data(deseq)
#'
#' ## DESeqAnalysis ====
#' mat <- degIntersection(deseq, return = "matrix")
#' head(mat)
#'
#' count <- degIntersection(deseq, return = "count")
#' head(count)
#'
#' ratio <- degIntersection(deseq, return = "ratio")
#' head(ratio)
NULL



#' @rdname degIntersection
#' @name degIntersection
#' @importFrom acidgenerics degIntersection
#' @usage degIntersection(object, ...)
#' @export
NULL



## Updated 2020-08-18.
`degIntersection,DESeqAnalysis` <-  # nolint
    function(
        object,
        i = NULL,
        direction = c("up", "down"),
        return = c("matrix", "count", "ratio")
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
        count <- sort(rowSums(mat), decreasing = TRUE)
        switch(
            EXPR = return,
            "matrix" = mat,
            "count" = count,
            "ratio" = count / length(list)
        )
    }



#' @rdname degIntersection
#' @export
setMethod(
    f = "degIntersection",
    signature = signature("DESeqAnalysis"),
    definition = `degIntersection,DESeqAnalysis`
)
