#' @name resultsDiff
#' @inherit AcidGenerics::resultsDiff
#' @note Updated 2020-08-05.
#'
#' @inheritParams AcidRoxygen::params
#' @inheritParams params
#'
#' @param col `character(1)`.
#' Column to use for difference calculation (subtraction).
#' Note that `x`/`i` represents `A` and `y`/`j` `B` in `A - B` calculation.
#'
#' @param deg `character(1)`.
#' - `"no"`: Return all genes, corresponding to original row names order.
#' - `"intersect"`: Return only genes called as DE in both contrasts.
#' - `"union"`: Return genes called as DE in either contrast.
#'
#' @examples
#' data(deseq)
#'
#' ## DESeqAnalysis ====
#' diff <- resultsDiff(deseq, i = 1L, j = 2L)
#' head(diff)
NULL



## Updated 2020-08-05.
`resultsDiff,DESeqAnalysis,missing` <- # nolint
    function(x, y = NULL, i, j, ...) {
        resultsDiff(
            x = results(x, i = i),
            y = results(x, i = j),
            ...
        )
    }



## Updated 2021-06-29.
`resultsDiff,DESeqResults,DESeqResults` <- # nolint
    function(x,
             y,
             col = c("log2FoldChange", "stat"),
             deg = c("no", "intersect", "union"),
             direction = c("both", "up", "down"),
             alphaThreshold = NULL,
             baseMeanThreshold = NULL,
             lfcThreshold = NULL) {
        col <- match.arg(col)
        deg <- match.arg(deg)
        direction <- match.arg(direction)
        assert(
            identical(dimnames(x), dimnames(y)),
            isSubset(col, colnames(x))
        )
        alert(sprintf("Calculating relative difference of {.var %s}.", col))
        diff <- x[[col]] - y[[col]]
        names(diff) <- rownames(x)
        if (!identical(deg, "no")) {
            degX <- deg(
                object = x,
                direction = direction,
                alphaThreshold = alphaThreshold,
                baseMeanThreshold = baseMeanThreshold,
                lfcThreshold = lfcThreshold
            )
            degY <- deg(
                object = y,
                direction = direction,
                alphaThreshold = alphaThreshold,
                baseMeanThreshold = baseMeanThreshold,
                lfcThreshold = lfcThreshold
            )
            genes <- switch(
                EXPR = deg,
                "intersect" = intersect(x = degX, y = degY),
                "union" = union(x = degX, y = degY)
            )
            if (!hasLength(genes)) {
                return(NULL)
            }
            alertInfo(sprintf(
                "Returning %s of DEGs (%d).",
                deg, length(genes)
            ))
            diff <- diff[genes]
        }
        diff
    }



#' @describeIn resultsDiff Passes arguments to `DESeqResults` method.
#' @export
setMethod(
    f = "resultsDiff",
    signature = signature(
        x = "DESeqAnalysis",
        y = "missingOrNULL"
    ),
    definition = `resultsDiff,DESeqAnalysis,missing`
)

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
