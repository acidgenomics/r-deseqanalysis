#' Relative difference of DESeq results
#'
#' @name resultsDiff
#' @note Updated 2019-12-17.
#'
#' @inheritParams params
#' @param col `character(1)`.
#'   Column to use for difference calculation (subtraction).
#'   Note that `x`/`i` represents `A` and `y`/`j` `B` in `A - B` calculation.
#' @param deg `character(1)`.
#'   - `"no"`: Return all genes, corresponding to original row names order.
#'   - `"intersect"`: Return only genes called as DE in both contrasts.
#'   - `"union"`: Return genes called as DE in either contrast.
#'
#' @return Named `numeric`.
#'   Names correspond to the genes (row names).
#'
#' @examples
#' data(deseq)
#'
#' ## DESeqResults ====
#' x <- results(deseq, i = 1L)
#' y <- results(deseq, i = 2L)
#' resultsDiff(x = x, y = y)
#'
#' ## DESeqAnalysis ====
#' object <- deseq
#' resultsDiff(object, i = 1L, j = 2L)
NULL



## Updated 2019-12-17.
`resultsDiff,DESeqResults,DESeqResults` <-
    function(
        x,
        y,
        col = c("log2FoldChange", "stat"),
        deg = c("no", "intersect", "union"),
        alpha = NULL,
        lfcThreshold = NULL,
        direction = c("both", "up", "down")
    ) {
        validObject(x)
        validObject(y)
        col <- match.arg(col)
        deg <- match.arg(deg)
        direction <- match.arg(direction)
        assert(
            identical(dimnames(x), dimnames(y)),
            isSubset(col, colnames(x))
        )
        message(sprintf("Calculating relative difference of '%s'.", col))
        diff <- x[[col]] - y[[col]]
        names(diff) <- rownames(x)
        if (!identical(deg, "no")) {
            degX <- deg(
                object = x,
                alpha = alpha,
                lfcThreshold = lfcThreshold,
                direction = direction
            )
            degY <- deg(
                object = y,
                alpha = alpha,
                lfcThreshold = lfcThreshold,
                direction = direction
            )
            genes <- switch(
                EXPR = deg,
                "intersect" = intersect(x = degX, y = degY),
                "union" = union(x = degX, y = degY)
            )
            if (!hasLength(genes)) {
                return(NULL)
            }
            diff <- diff[genes]
        }
        diff
    }



#' @rdname resultsDiff
#' @export
setMethod(
    f = "resultsDiff",
    signature = signature(
        x = "DESeqResults",
        y = "DESeqResults"
    ),
    definition = `resultsDiff,DESeqResults,DESeqResults`
)



## Updated 2019-12-17.
`resultsDiff,DESeqAnalysis,missing` <-
    function(x, y = NULL, i, j, ...) {
        stop("NOT DEFINED YET")
    }



#' @rdname resultsDiff
#' @export
setMethod(
    f = "resultsDiff",
    signature = signature(
        x = "DESeqAnalysis",
        y = "missingOrNULL"
    ),
    definition = `resultsDiff,DESeqAnalysis,missing`
)
