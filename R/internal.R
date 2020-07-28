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



#' Map contrast vector to coefficient
#'
#' @note Updated 2019-09-17.
#' @noRd
.contrast2coef <- function(contrast, resultsNames) {
    assert(
        isCharacter(contrast),
        hasLength(contrast, n = 3L),
        isCharacter(resultsNames)
    )
    factor <- contrast[[1L]]
    numerator <- contrast[[2L]]
    denominator <- contrast[[3L]]
    coef <- match(
        x = paste(factor, numerator, "vs", denominator, sep = "_"),
        table = resultsNames
    )
    assert(isInt(coef), !is.na(coef))
    message(sprintf("Contrast: %s\nCoef: %d", resultsNames[[coef]], coef))
    coef
}



## Updated 2019-12-13.
.degPerContrast <- function(
    object,
    alpha = NULL,
    lfcThreshold = NULL,
    direction = c("both", "up", "down"),
    n = FALSE
) {
    assert(
        is(object, "DESeqAnalysis"),
        isFlag(n)
    )
    direction <- match.arg(direction)
    suppressMessages({
        mapply(
            i = resultsNames(object),
            MoreArgs = list(object = object),
            FUN = function(i, object) {
                if (isSubset(direction, c("both", "down"))) {
                    down <- deg(
                        object = object,
                        i = i,
                        direction = "down",
                        alpha = alpha,
                        lfcThreshold = lfcThreshold
                    )
                    if (isTRUE(n)) {
                        down <- length(down)
                    }
                }
                if (isSubset(direction, c("both", "up"))) {
                    up <- deg(
                        object = object,
                        i = i,
                        direction = "up",
                        alpha = alpha,
                        lfcThreshold = lfcThreshold
                    )
                    if (isTRUE(n)) {
                        up <- length(up)
                    }
                }
                if (isTRUE(n)) {
                    switch(
                        EXPR = direction,
                        "both" = c(down = down, up = up),
                        "down" = c(down = down),
                        "up" = c(up = up)
                    )
                } else {
                    switch(
                        EXPR = direction,
                        "both" = list(down = down, up = up),
                        "down" = list(down = down),
                        "up" = list(up = up)
                    )
                }
            },
            SIMPLIFY = FALSE,
            USE.NAMES = TRUE
        )
    })
}



## Updated 2019-07-23.
.ddsMsg <- function() {
    message(sprintf(
        "Generating DESeqDataSet with DESeq2 %s.",
        packageVersion("DESeq2")
    ))
}



## Updated 2019-11-12.
.joinCounts <- function(
    object,  # nolint
    DESeqDataSet   # nolint
) {
    assert(
        is(object, "DataFrame"),
        is(DESeqDataSet, "DESeqDataSet"),
        identical(
            x = rownames(object),
            y = rownames(DESeqDataSet)
        ),
        areDisjointSets(
            x = colnames(object),
            y = colnames(DESeqDataSet)
        )
    )
    validObject(object)
    validObject(DESeqDataSet)
    message("Joining size factor adjusted normalized counts.")
    counts <- counts(DESeqDataSet, normalized = TRUE)
    out <- cbind(object, counts)
    ## Ensure we're not changing the object class on return.
    ## This can happen for DESeqResults, which will coerce to DataFrame.
    if (!identical(x = class(object), y = class(out))) {
        out <- as(out, Class = class(object)[[1L]])
    }
    validObject(out)
    out
}



## Join the row annotations.
##
## DESeq2 includes additional columns in `rowData()` that aren't informative for
## a user, and doesn't need to be included in the tables. Instead, only keep
## informative columns that are character or factor. Be sure to drop complex,
## non-atomic columns (e.g. list, S4) that are allowed in GRanges/DataFrame but
## will fail to write to disk as CSV. Note that we're using `decode()` here to
## handle S4 Rle columns from the Genomic Ranges.
##
## Updated 2019-11-12.
.joinRowData <- function(
    object,  # nolint
    DESeqDataSet   # nolint
) {
    assert(
        is(object, "DataFrame"),
        is(DESeqDataSet, "DESeqDataSet"),
        identical(
            x = rownames(object),
            y = rownames(DESeqDataSet)
        ),
        areDisjointSets(
            x = colnames(object),
            y = colnames(DESeqDataSet)
        )
    )
    validObject(object)
    validObject(DESeqDataSet)
    message("Joining row annotations.")
    ## SummarizedExperiment inconsistently handles rownames on rowData.
    ## Ensure they are set here before continuing.
    rownames <- rownames(DESeqDataSet)
    rowData <- rowData(DESeqDataSet)
    rownames(rowData) <- rownames
    rowData <- decode(rowData)
    keep <- vapply(
        X = rowData,
        FUN = function(x) {
            is.character(x) || is.factor(x)
        },
        FUN.VALUE = logical(1L)
    )
    if (!any(keep)) {
        stop(
            "No suitable row annotations detected.\n",
            "Check 'rowData()' of DESeqDataSet."
        )
    }
    rowData <- rowData[, keep, drop = FALSE]
    ## Drop any remaining blacklisted columns. These columsn aren't useful in
    ## the downstream export to CSV format.
    blacklist <- "seqCoordSystem"
    keep <- !colnames(rowData) %in% blacklist
    rowData <- rowData[, keep, drop = FALSE]
    assert(
        all(vapply(
            X = rowData,
            FUN = is.atomic,
            FUN.VALUE = logical(1L)
        )),
        hasLength(rowData),
        identical(rownames(object), rownames(rowData)),
        areDisjointSets(colnames(object), colnames(rowData))
    )
    out <- cbind(object, rowData)
    ## Ensure we're not changing the object class on return.
    ## This can happen for DESeqResults, which will coerce to DataFrame.
    if (!identical(x = class(object), y = class(out))) {
        out <- as(out, Class = class(object)[[1L]])
    }
    validObject(out)
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
