#' @name degIntersection
#' @inherit acidgenerics::degIntersection
#' @note Updated 2020-08-18.
#'
#' @inheritParams acidroxygen::params
#' @param i `character`, `numeric`, or `NULL`.
#'   Names or range of results.
#'   If set `NULL`, include all results.
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
        return = c("matrix", "count", "ratio"),
        ...
    ) {
        return <- match.arg(return)
        if (is.null(i)) i <- resultsNames(object)
        list <- lapply(
            X = i,
            FUN = deg,
            object = object,
            ... = ...
        )
        mat <- intersectionMatrix(list)
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
