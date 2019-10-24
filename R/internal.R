#' Calculate a numeric vector to define the colors.
#' @details
#' - `-1`: downregulated
#' -  `0`: not significant
#' -  `1`: upregulated
#' @noRd
## Updated 2019-07-23.
.addIsDECol <- function(
    data,
    testCol = "padj",
    alpha,
    lfcCol = "log2FoldChange",
    lfcThreshold
) {
    ## test: P value or S value
    test <- data[[testCol]]
    ## lfc: log2 fold change cutoff
    lfc <- data[[lfcCol]]
    isDE <- mapply(
        test = test,
        lfc = lfc,
        FUN = function(test, lfc) {
            if (any(is.na(c(test, lfc)))) {
                ## nonsignificant
                0L
            } else if (test < alpha & lfc > lfcThreshold) {
                ## upregulated
                1L
            } else if (test < alpha & lfc < -lfcThreshold) {
                ## downregulated
                -1L
            } else {
                0L
            }
        },
        SIMPLIFY = TRUE,
        USE.NAMES = FALSE
    )
    isDE <- as.factor(isDE)
    data[["isDE"]] <- isDE
    data
}



## Updated 2019-07-23.
.ddsMsg <- function() {
    message(sprintf(
        "Generating DESeqDataSet with DESeq2 %s.",
        packageVersion("DESeq2")
    ))
}



#' Extract the useful row data from object
#'
#' Intentionally drop `logical` and `numeric` columns, which contain values used
#' internally by DESeq2.
#'
#' @note Updated 2019-10-24.
#' @noRd
#'
#' @param object `DESeqAnalysis` or `DESeqAnalysisList`.
#'
#' @examples
#' data <- .usefulRowData(object)
.usefulRowData <- function(object) {
    if (is(object, "DESeqAnalysisList")) {
        object <- object[[1L]]
    }
    assert(is(object, "DESeqAnalysis"))
    dds <- as(object, "DESeqDataSet")
    data <- rowData(dds)
    assert(
        is(data, "DataFrame"),
        hasLength(data)
    )
    keep <- !bapply(X = data, FUN = isAny, classes = c("logical", "numeric"))
    out <- data[, keep, drop = FALSE]
    out
}



## Updated 2019-07-23.
.transformCountsAxisLabel <- function(object) {
    paste(transformType(object), "counts (log2)")
}



#' Determine which results slot to use
#'
#' @note Updated 2019-10-24.
#' @noRd
#'
#' @param object `DESeqAnalysis` or `DESeqAnalysisList`.
#' @param value `character(1)`.
#'   `DESeqResults` column name.
#'
#' @return `character(1)`.
#'   `DESeqResults` slot name. Either "results" or "lfcShrink"
#'
#' @examples
#' .whichResults(object, value = "log2FoldChange")
.whichResults <- function(object, value) {
    if (is(object, "DESeqAnalysisList")) {
        object <- object[[1L]]
    }
    assert(
        is(object, "DESeqAnalysis"),
        isCharacter(value)
    )
    if (
        identical(value, "log2FoldChange") &&
        hasLength(slot(object, "lfcShrink"))
    ) {
        slot <- "lfcShrink"
    } else {
        slot <- "results"
    }
    slot
}
