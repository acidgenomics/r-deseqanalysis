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



## Updated 2019-07-23.
.transformCountsAxisLabel <- function(object) {
    paste(transformType(object), "counts (log2)")
}
