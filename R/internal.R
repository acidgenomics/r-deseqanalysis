#' Calculate a numeric vector to define the colors
#'
#' @note Updated 2020-08-04.
#' @noRd
#'
#' @details
#' - test: P value or S value.
#' - lfc: log2 fold change cutoff.
#'
#' @return `integer`.
#' - `-1`: downregulated
#' -  `0`: not significant
#' -  `1`: upregulated
.addIsDegCol <- function(
    data,
    alphaCol = "padj",
    alphaThreshold,
    lfcCol = "log2FoldChange",
    lfcThreshold,
    baseMeanCol = "baseMean",
    baseMeanThreshold
) {
    cols <- c(alphaCol, lfcCol, baseMeanCol)
    assert(isSubset(cols, colnames(data)))
    isDeg <- mapply(
        test = data[[alphaCol]],
        lfc = data[[lfcCol]],
        baseMean = data[[baseMeanCol]],
        MoreArgs = list(
            alphaThreshold = alphaThreshold,
            lfcThreshold = lfcThreshold,
            baseMeanThreshold = baseMeanThreshold
        ),
        FUN = function(
            test,
            alphaThreshold,
            lfc,
            lfcThreshold,
            baseMean,
            baseMeanThreshold
        ) {
            if (
                any(is.na(c(test, lfc, baseMean))) ||
                test >= alphaThreshold ||
                baseMean < baseMeanThreshold
            ) {
                return(0L)
            }
            if (lfc >= lfcThreshold) {
                1L
            } else if (lfc <= -lfcThreshold) {
                -1L
            } else {
                0L
            }
        },
        SIMPLIFY = TRUE,
        USE.NAMES = FALSE
    )
    data[["isDeg"]] <- as.factor(isDeg)
    data
}



#' Map contrast vector to coefficient
#'
#' @note Updated 2020-08-13.
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
    dl(c(
        "contrast" = as.character(resultsNames[[coef]]),
        "coef" = as.character(coef)
    ))
    coef
}



## Updated 2020-08-04.
.ddsMsg <- function() {
    alertInfo(sprintf(
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



## FIXME CAN WE SHOW DOWN / UP MORE CLEARLY?

## Updated 2021-03-03.
.thresholdLabel <- function(
    data,
    direction,
    alphaThreshold,
    lfcShrinkType,
    lfcThreshold,
    baseMeanThreshold
) {
    assert(
        is(data, "DataFrame"),
        assert(isSubset("isDeg", colnames(data)))
    )




    sep <- "; "
    if (is.null(n)) {
        x <- NULL
    } else {
        x <- paste0("n = ", n, sep)
    }
    if (direction != "both") {
        x <- paste0(x, "direction: ", direction, sep)
    }
    x <- paste0(x, "alpha < ", alphaThreshold)
    if (lfcThreshold > 0L) {
        x <- paste0(x, sep, "lfc >= ", lfcThreshold)
    }
    if (lfcShrinkType != "unshrunken") {
        x <- paste0(x, sep, "lfcShrink: ", lfcShrinkType)
    }
    if (baseMeanThreshold > 1L) {
        x <- paste0(x, sep, "baseMean >= ", baseMeanThreshold)
    }
    x
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
