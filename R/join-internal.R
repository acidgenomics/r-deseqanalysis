## Used internally by:
## - `resultsMatrix`
## - `resultsTables`
## - `topTables`



## Updated 2019-07-23.
.joinCounts <- function(
    DESeqResults,  # nolint
    DESeqDataSet   # nolint
) {
    assert(
        is(DESeqResults, "DESeqResults"),
        is(DESeqDataSet, "DESeqDataSet"),
        identical(
            x = rownames(DESeqResults),
            y = rownames(DESeqDataSet)
        ),
        areDisjointSets(
            x = colnames(DESeqResults),
            y = colnames(DESeqDataSet)
        )
    )
    validObject(DESeqResults)
    validObject(DESeqDataSet)
    message("Joining size factor adjusted normalized counts.")
    counts <- counts(DESeqDataSet, normalized = TRUE)
    out <- cbind(DESeqResults, counts)
    out <- as(out, "DESeqResults")
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
## Updated 2019-10-15.
.joinRowData <- function(
    DESeqResults,  # nolint
    DESeqDataSet   # nolint
) {
    assert(
        is(DESeqResults, "DESeqResults"),
        is(DESeqDataSet, "DESeqDataSet"),
        identical(
            x = rownames(DESeqResults),
            y = rownames(DESeqDataSet)
        ),
        areDisjointSets(
            x = colnames(DESeqResults),
            y = colnames(DESeqDataSet)
        )
    )
    validObject(DESeqResults)
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
    assert(
        all(vapply(
            X = rowData,
            FUN = is.atomic,
            FUN.VALUE = logical(1L)
        )),
        hasLength(rowData),
        identical(rownames(DESeqResults), rownames(rowData)),
        areDisjointSets(colnames(DESeqResults), colnames(rowData))
    )
    out <- cbind(DESeqResults, rowData)
    out <- as(out, "DESeqResults")
    validObject(out)
    out
}
