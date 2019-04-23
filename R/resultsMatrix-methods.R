#' DESeqResults matrix
#'
#' Generate an aggregate matrix of DESeqResults values.
#'
#' @name resultsMatrix
#'
#' @param object `DESeqAnalysis`.
#' @param value `character(1)`.
#'   Value type to return. Corresponds to supported `DESeqResults` column:
#'
#'   - `log2FoldChange`: log2 fold change. This will return *shrunken* LFC
#'     values if they are slotted in the `DESeqAnalysis` object.
#'   - `stat`: Wald test statistic
#'   - `padj`: BH adjusted *P* value
#'
#' @return `matrix`.
#'
#' @examples
#' data(deseq)
#' x <- resultsMatrix(deseq)
#' head(x)
NULL



#' @rdname resultsMatrix
#' @name resultsMatrix
#' @importFrom bioverbs resultsMatrix
#' @export
NULL



resultsMatrix.DESeqAnalysis <- function(
    object,
    value = c("log2FoldChange", "stat", "padj")
) {
    assert(is(object, "DESeqAnalysis"))
    value <- match.arg(value)

    # Get appropriate list of `DESeqResults`.
    # Use shrunken LFC values, if defined.
    # Otherwise, just pull values from `results()` return.
    if (
        value == "log2FoldChange" &&
        is.list(slot(object, "lfcShrink"))
    ) {
        slotName <- "lfcShrink"
    } else {
        slotName <- "results"
    }

    message(paste(
        "Generating results matrix from",
        slotName, "slot using", value, "column."
    ))

    results <- slot(object, name = slotName)
    assert(
        is.list(results),
        hasValidNames(results)
    )

    list <- lapply(
        X = results,
        col = value,
        FUN = function(data, col) data[[col]]
    )
    unlist <- unlist(list, recursive = FALSE, use.names = FALSE)
    mat <- matrix(
        data = unlist,
        ncol = length(list),
        byrow = FALSE,
        dimnames = list(
            rownames(results[[1L]]),
            names(list)
        )
    )

    # Double check that our unlist operation is correct.
    assert(
        identical(
            unname(results[[1L]][[value]]),
            unname(mat[, 1L, drop = TRUE])
        )
    )

    # Stash useful metadata in the object.
    attr(mat, which = "DESeqAnalysis") <-
        list(
            version = packageVersion("DESeqAnalysis"),
            date = Sys.Date(),
            slotName = slotName,
            value = value
        )

    mat
}



#' @rdname resultsMatrix
#' @export
setMethod(
    f = "resultsMatrix",
    signature = signature("DESeqAnalysis"),
    definition = resultsMatrix.DESeqAnalysis
)
