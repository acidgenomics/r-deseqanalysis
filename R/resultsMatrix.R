# Consider warning/erroring if object only contains a single results contrast.



#' DESeqResults matrix
#'
#' Generate an aggregate matrix of DESeqResults values.
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
#' stop("NOT SUPPORTED YET")
resultsMatrix <- function(
    object,
    value = c("log2FoldChange", "stat", "padj")
) {
    assert(is(object, "DESeqAnalysis"))
    value <- match.arg(value)

    # Get appropriate list of `DESeqResults`.
    if (value == "log2FoldChange") {
        # Use shrunken LFC values, if defined.
    } else {
        # Otherwise, just pull values from `results()` return.
    }

}
