#' @name deg
#' @inherit basejump::deg
#' @examples
#' data(deseq)
#'
#' ## DESeqAnalysis ====
#' deg(deseq)
NULL



# Get differential expressed genes (DEGs) from DESeqResults table.
# Note that we're not sorting the identifiers here by LFC or P value.
# It's just performing a simple subset to get the identifiers as a character.
deg.DESeqResults <- function(
    object,
    alpha = NULL,
    lfcThreshold = NULL,
    direction = c("both", "up", "down")
) {
    validObject(object)
    if (is.null(alpha)) {
        alpha <- metadata(object)[["alpha"]]
    } else {
        warning("Applying a post hoc alpha cutoff is not recommended.")
    }
    assertIsAlpha(alpha)
    if (is.null(lfcThreshold)) {
        lfcThreshold <- metadata(object)[["lfcThreshold"]]
    } else {
        warning("Applying a post hoc LFC threshold cutoff is not recommended.")
    }
    assert_is_a_number(lfcThreshold)
    assert_all_are_non_negative(lfcThreshold)
    direction <- match.arg(direction)

    # Define symbols to use in dplyr calls below.
    padj <- sym("padj")
    lfc <- sym("log2FoldChange")

    # Coerce to minimal tibble.
    data <- as(object, "tbl_df")
    cols <- c("rowname", "log2FoldChange", "padj")
    assert_is_subset(cols, colnames(data))
    data <- select(data, !!!syms(cols))

    # Apply alpha cutoff.
    data <- filter(data, !!padj < !!alpha)

    # Apply LFC threshold cutoff.
    if (lfcThreshold > 0L) {
        data <- filter(
            data,
            !!lfc > UQ(lfcThreshold) | !!lfc < -UQ(lfcThreshold)
        )
    }

    # Apply directional filtering.
    if (direction == "up") {
        data <- filter(data, !!lfc > 0L)
    } else if (direction == "down") {
        data <- filter(data, !!lfc < 0L)
    }

    deg <- pull(data, "rowname")

    if (!has_length(deg)) {
        warning("No significant DEGs detected.")
    } else {
        message(paste(length(deg), "differentially expressed genes detected."))
    }

    deg
}



#' @rdname deg
#' @export
setMethod(
    f = "deg",
    signature = signature("DESeqResults"),
    definition = deg.DESeqResults
)




deg.DESeqAnalysis <- function(
    object,
    results = 1L,
    direction = c("both", "up", "down")
) {
    results <- .matchResults(object = object, results = results)
    direction <- match.arg(direction)
    deg(object = results, direction = direction)
}



#' @rdname deg
#' @export
setMethod(
    f = "deg",
    signature = signature("DESeqAnalysis"),
    definition = deg.DESeqAnalysis
)
