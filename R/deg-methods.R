#' @name deg
#' @inherit basejump::deg
#' @inheritParams basejump::params
#' @inheritParams params
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
    assert(containsAlpha(alpha))
    if (is.null(lfcThreshold)) {
        lfcThreshold <- metadata(object)[["lfcThreshold"]]
    } else {
        warning("Applying a post hoc LFC threshold cutoff is not recommended.")
    }
    assert(
        isNumber(lfcThreshold),
        isNonNegative(lfcThreshold)
    )
    direction <- match.arg(direction)

    # Define symbols to use in dplyr calls below.
    alphaCol <- sym("padj")
    lfcCol <- sym("log2FoldChange")

    # Coerce to minimal tibble.
    data <- as(object, "tbl_df")
    data <- select(data, !!!syms(c("rowname", "log2FoldChange", "padj")))

    # Apply alpha cutoff.
    data <- filter(data, !!alphaCol < !!alpha)

    # Apply LFC threshold cutoff.
    if (lfcThreshold > 0L) {
        data <- filter(
            data,
            !!lfcCol > UQ(lfcThreshold) | !!lfcCol < -UQ(lfcThreshold)
        )
    }

    # Apply directional filtering.
    if (direction == "up") {
        data <- filter(data, !!lfcCol > 0L)
    } else if (direction == "down") {
        data <- filter(data, !!lfcCol < 0L)
    }

    # Arrange table by adjusted P value.
    data <- arrange(data, !!alphaCol)

    deg <- pull(data, "rowname")

    if (!hasLength(deg)) {
        warning("No significant DEGs detected.")
    } else {
        message(paste(
            length(deg),
            switch(
                EXPR = direction,
                up = "upregulated",
                down = "downregulated",
                both = "differentially expressed"
            ),
            "genes detected."
        ))
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
    suppressMessages(
        results <- .matchResults(object = object, results = results)
    )
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
