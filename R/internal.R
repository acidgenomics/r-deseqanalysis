## FIXME Should we just rank by adjusted p value here? simpler....
##       Or allow the user to set which parameter they want to use for the
##       ranking here...
## FIXME Rework this, standardizing for plotMA and plotVolcano...
## FIXME Ensure this is sorted by rank.
## FIXME How to handle sort if padj is not significant or NA?



#' Prepare `DESeqResults` data for plot
#'
#' @details
#' - `test`: P value or S value.
#' - `lfc`: log2 fold change cutoff.
#'
#' @section `isDeg`:
#'
#' Calculate an `integer factor`, used to define the color mappings:
#'
#' - `-1`: downregulated
#' -  `0`: not significant
#' -  `1`: upregulated
#'
#' @note Updated 2021-06-28.
#' @noRd
#'
#' @return `DataFrame`.
.prepareResultsForPlot <- function(
    object,
    direction,
    alphaThreshold,
    baseMeanThreshold,
    lfcThreshold
) {
    assert(is(object, "DESeqResults"))
    data <- as(object, "DataFrame")
    colnames(data) <- camelCase(colnames(data), strict = TRUE)
    baseMeanCol <- "baseMean"
    lfcCol <- "log2FoldChange"
    alphaCol <- ifelse(
        test = isTRUE(isSubset("svalue", names(object))),
        yes = "svalue",
        no = "padj"
    )
    isDegCol <- "isDeg"
    ## NOTE `lfcShrink()` doesn't return `stat` column.
    rankCol <- ifelse(
        test = isTRUE(isSubset("stat", names(object))),
        yes = "stat",
        no = lfcCol
    )
    assert(isSubset(
        x = c(alphaCol, baseMeanCol, lfcCol, rankCol),
        y = colnames(data)
    ))
    ## Remove genes with very low expression.
    keep <- which(data[[baseMeanCol]] >= baseMeanThreshold)
    data <- data[keep, , drop = FALSE]
    ## Apply directional filtering, if desired.
    switch(
        EXPR = direction,
        "up" = {
            keep <- which(data[[lfcCol]] > 0L)
            data <- data[keep, , drop = FALSE]
        },
        "down" = {
            keep <- which(data[[lfcCol]] < 0L)
            data <- data[keep, , drop = FALSE]
        }
    )
    ## Check for no genes passing cutoffs and early return.
    if (!hasRows(data)) {
        alertWarning("No genes passed cutoffs.")
        return(NULL)
    }
    data[[isDegCol]] <- as.factor(mapply(
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
    ))
    data[["rankScore"]] <- abs(data[[rankCol]])
    data[["rankScore"]][data[["isDeg"]] == 0L] <- NA
    data <- data[order(data[["rankScore"]], decreasing = TRUE), , drop = FALSE]

    ## FIXME This needs to only rank significant genes.
    ## Otherwise we want to censors
    data[["rank"]] <- seq_len(nrow(data))
    assert(isSubset(
        x = c("isDeg", "rank", "rankScore"),
        y = colnames(data)
    ))




    ## FIXME Need to re-sort again here...














    ## FIXME Need to support these better...
    metadata(data)[["isDegCol"]] <- isDegCol

    data
}



#' Coerce a DESeqResults object to named list
#'
#' @note Updated 2019-07-23.
#' @noRd
#'
#' @details
#' Note that this will automatically assign name.
.coerceResultsToList <- function(from) {
    assert(is(from, "DESeqResults"))
    to <- list(from)
    names(to) <- makeNames(contrastName(from))
    to
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



## FIXME Rework this, using metadata stash approach instead...
## FIXME We don't need to pass lfcShrinkType here correct??

#' Threshold label that goes in subtitle for plot on DESeqResults
#'
#' @note Updated 2021-06-28.
#' @noRd
.thresholdLabel <- function(
    object,
    direction,
    alphaThreshold,
    lfcShrinkType,
    lfcThreshold,
    baseMeanThreshold
) {
    ## FIXME Should we no longer allow DESeqAnalysis here?
    assert(isAny(object, c("DESeqAnalysis", "DESeqResults")))
    x <- character()
    sep <- "; "
    if (is(object, "DESeqResults")) {
        n <- vapply(
            X = switch(
                EXPR = direction,
                "both" = c("up", "down"),
                direction
            ),
            FUN = function(direction) {
                length(deg(
                    object = object,
                    direction = direction,
                    alphaThreshold = alphaThreshold,
                    lfcThreshold = lfcThreshold,
                    baseMeanThreshold = baseMeanThreshold,
                    quiet = TRUE
                ))
            },
            FUN.VALUE = integer(1L),
            USE.NAMES = TRUE
        )
        x <- paste0(x, paste("n", "=", sum(n)))
        if (direction == "both" && sum(n) > 0L) {
            x <- paste0(x, sep, paste(names(n), n, sep = ": ", collapse = sep))
        } else {
            x <- paste0(x, " (", direction, ")")
        }
        x <- paste0(x, sep)
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
