## FIXME ADD SUPPORT FOR DESEQRESULTS PASSED IN DIRECTLY HERE???



#' @name degIntersection
#' @inherit AcidGenerics::degIntersection return title
#' @note Updated 2021-03-08.
#'
#' @param object,...
#'   Object and/or other objects of same class.
#'   Function is parameterized.
#' @param i `character`, `numeric`, or `NULL`.
#'   Names or range of results.
#'   If set `NULL`, include all results per object.
#'   When passing in multiple objects, specify the desired results as a `list`
#'   with length matching the number of input objects, containing either
#'   `character` or `numeric` corresponding to each object.
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



## FIXME CONSIDER REWORKING THIS TO DISPATCH ON DESEQRESULTS METHOD...

## Updated 2021-03-08.
`degIntersection,DESeqAnalysis` <-  # nolint
    function(
        object,
        ...,
        i = NULL,
        alphaThreshold = NULL,
        lfcThreshold = NULL,
        baseMeanThreshold = NULL,
        direction = c("up", "down"),
        return = c("matrix", "count", "ratio", "names")
    ) {
        objects <- append(x = list(object), values = list(...))
        if (is.null(i)) {
            i <- lapply(X = objects, FUN = resultsNames)
        }
        assert(areSameLength(objects, i))
        if (is.null(alphaThreshold)) {
            alphaThreshold <- alphaThreshold(object)
        }
        if (is.null(lfcThreshold)) {
            lfcThreshold <- lfcThreshold(object)
        }
        if (is.null(baseMeanThreshold)) {
            baseMeanThreshold <- baseMeanThreshold(object)
        }
        direction <- match.arg(direction)
        return <- match.arg(return)
        x <- mapply(
            object = objects,
            i = i,
            MoreArgs = list(
                "alphaThreshold" = alphaThreshold,
                "lfcThreshold" = lfcThreshold,
                "baseMeanThreshold" = baseMeanThreshold,
                "direction" = direction,
                "return" = return
            ),
            FUN = function(
                object,
                i,
                alphaThreshold,
                lfcThreshold,
                baseMeanThreshold,
                direction,
                return
            ) {
                validObject(object)
                x <- lapply(
                    X = i,
                    FUN = deg,
                    object = object,
                    alphaThreshold = alphaThreshold,
                    lfcThreshold = lfcThreshold,
                    baseMeanThreshold = baseMeanThreshold,
                    direction = direction,
                    quiet = TRUE
                )
                names(x) <- i
                x
            },
            SIMPLIFY = FALSE,
            USE.NAMES = FALSE
        )
        x <- unlist(x = x, recursive = FALSE, use.names = TRUE)
        assert(hasNames(x))
        if (any(duplicated(names(x)))) {
            dupes <- names(x)[duplicated(names(x))]
            stop(sprintf(
                "%d duplicate %s: %s.",
                length(dupes),
                ngettext(
                    n = length(dupes),
                    msg1 = "contrast",
                    msg2 = "contrasts"
                ),
                toString(dupes, width = 200L)
            ))
        }
        mat <- intersectionMatrix(x)
        alert(sprintf(
            "Returning intersection %s of %s %s-regulated DEGs.",
            return, nrow(mat), direction
        ))
        count <- rowSums(mat)
        mode(count) <- "integer"
        switch(
            EXPR = return,
            "count" = count,
            "matrix" = mat,
            "names" = {
                x <- apply(X = mat, MARGIN = 1L, FUN = all)
                x <- names(x)[x]
                x
            },
            "ratio" = {
                count / length(x)
            }
        )
    }



#' @rdname degIntersection
#' @export
setMethod(
    f = "degIntersection",
    signature = signature("DESeqAnalysis"),
    definition = `degIntersection,DESeqAnalysis`
)
