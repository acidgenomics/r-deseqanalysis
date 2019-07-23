#' @name resultsTables
#' @inherit bioverbs::resultsTables
#'
#' @inheritParams basejump::params
#' @inheritParams params
#' @param ... Additional arguments.
#'
#' @details
#' Generate tables summarizing the differential expression, with subsets for
#' differentially expressed genes (DEGs). DEG tables (i.e. everything except the
#' `all` table), are arranged by adjusted *P* value.
#'
#' @note Do not apply post hoc log fold change cutoffs.
#'
#' @section Tables:
#'
#' - `all`: All genes, including genes without an adjusted *P* value. This table
#'   is unmodified, and the rows have not been re-arranged or subset. It is
#'   suitable for gene set enrichment analysis (GSEA).
#' - `up`: Upregulated genes.
#' - `down`: Downregulated genes.
#' - `both`: Bi-directional DEGs (up- and down-regulated). This table can be
#'   used for overrepresentation testing but should NOT be used for GSEA.
#'
#' @param rowData `logical(1)`.
#'   Join the row annotations.
#' @param counts `logical(1)`.
#'   Join the size-factor adjusted normalized counts.
#' @param return `character(1)`.
#'   Type of data frame to return in the list. Uses
#'   [match.arg()][base::match.arg]. Note that `DataFrame` option will return
#'   with row names, whereas `tbl_df` option will return with `"rowname"`
#'   column.
#'
#' @return `list`.
#' Named list containing subsets of `DESeqResults`.
#'
#' @examples
#' data(deseq)
#'
#' ## DESeqAnalysis ====
#' x <- resultsTables(deseq, results = 1L)
#' print(x)
#'
#' ## DESeqResults ====
#' Use of DESeqAnalysis is encouraged instead of this approach.
#' res <- results(deseq, results = 1L)
#' dds <- as(deseq, "DESeqDataSet")
#' x <- resultsTables(object = res, counts = dds)
NULL



#' @rdname resultsTables
#' @name resultsTables
#' @importFrom bioverbs resultsTables
#' @usage resultsTables(object, ...)
#' @export
NULL



## Updated 2019-07-23.
.joinCounts <- function(results, counts) {
    assert(
        is(results, "DESeqResults"),
        isAny(counts, c("DESeqDataSet", "matrix"))
    )
    validObject(results)
    validObject(counts)
    if (is(counts, "DESeqDataSet")) {
        message("Joining size factor adjusted normalized counts.")
        counts <- counts(counts, normalized = TRUE)
    } else {
        message("Joining counts.")
    }
    assert(
        is.matrix(counts),
        identical(rownames(results), rownames(counts)),
        areDisjointSets(colnames(results), colnames(counts))
    )
    out <- cbind(results, counts)
    out <- as(out, "DESeqResults")
    validObject(out)
    out
}



## Updated 2019-07-23.
.joinRowData <- function(results, rowData) {
    assert(
        is(results, "DESeqResults"),
        isAny(rowData, c("SummarizedExperiment", "DataFrame"))
    )
    validObject(results)
    validObject(rowData)
    if (is(rowData, "SummarizedExperiment")) {
        ## SummarizedExperiment inconsistently handles rownames on rowData.
        ## Ensure they are set here before continuing.
        rownames <- rownames(rowData)
        rowData <- rowData(rowData)
        rownames(rowData) <- rownames
    }
    message("Joining row annotations.")
    rowData <- decode(rowData)
    keep <- vapply(
        X = rowData,
        FUN = function(x) {
            is.character(x) || is.factor(x)
        },
        FUN.VALUE = logical(1L)
    )
    rowData <- rowData[, keep, drop = FALSE]
    assert(
        all(vapply(
            X = rowData,
            FUN = is.atomic,
            FUN.VALUE = logical(1L)
        )),
        isNonEmpty(rowData),
        identical(rownames(results), rownames(rowData)),
        areDisjointSets(colnames(results), colnames(rowData))
    )
    out <- cbind(results, rowData)
    out <- as(out, "DESeqResults")
    validObject(out)
    out
}



## FIXME Add counts support
## FIXME Inform the user about removal of Dropbox support.

## FIXME Make the alpha mode automatic.
## Detect it internally instead.

## bcbioRNASeq v0.2 release series defaults:
## https://github.com/hbc/bcbioRNASeq/blob/v0.2.10/R/resultsTables-methods.R
## nolint start
## - alpha
## - lfcThreshold = 0L
## - summary = TRUE
## - write = FALSE
## - headerLevel = 2L
## - dir = "."
## - dropboxDir = NULL
## - rdsToken = NULL
## nolint end

## counts
## matrix or DESeqDataSet

## Note that this method is used in bcbioRNASeq F1000 paper.
## Updated 2019-07-23.
`resultsTables,DESeqResults` <-  # nolint
    function(
        object,
        counts = NULL,
        return = c("tbl_df", "DataFrameList"),
        ...
    ) {
        validObject(object)
        assert(isAny(counts, c("DESeqDataSet", "NULL")))
        return <- match.arg(return)

        ## Legacy bcbioRNASeq arguments ----------------------------------------
        call <- match.call()

        ## alpha
        if (isSubset("alpha", names(call))) {
            error("Post-hoc alpha level filtering is no longer supported.")
        }

        ## lfcThreshold
        if (isSubset("lfcThreshold", names(call))) {
            error("Post-hoc LFC threshold filtering is no longer supported.")
        }

        ## summary

        ## dir
        ## write
        ## FIXME Encourage export instead.

        ## headerLevel

        ## dropboxDir
        ## rdsToken

        ## Join row data and counts from DESeqDataSet.
        if (is(counts, "DESeqDataSet")) {
            object <- .joinRowData(results = object, rowData = counts)
        }
        if (!is.null(counts)) {
            object <- .joinCounts(results = object, counts = counts)
        }

        ## Get the DEG character vectors, which we'll use against the rownames.
        both <- deg(object, direction = "both")
        ## Early return with warning if there are not DEGs.
        if (!hasLength(both)) {
            warning(paste(
                deparse(results),
                "does not contain any DEGs. Skipping."
            ), call. = FALSE)
            return(invisible())
        }
        up <- deg(object, direction = "up")
        down <- deg(object, direction = "down")

        ## Prepare the return list.
        out <- list(
            all = object,
            up = object[up, , drop = FALSE],
            down = object[down, , drop = FALSE],
            both = object[both, , drop = FALSE]
        )

        ## Filter out empty up/down tables.
        out <- Filter(f = hasRows, x = out)

        switch(
            EXPR = return,
            DataFrameList = DataFrameList(out),
            tbl_df = lapply(out, as_tibble)
        )
    }



#' @rdname resultsTables
#' @export
setMethod(
    f = "resultsTables",
    signature = signature("DESeqResults"),
    definition = `resultsTables,DESeqResults`
)



## Updated 2019-07-23.
`resultsTables,DESeqAnalysis` <-  # nolint
    function(
        object,
        results,
        lfcShrink = TRUE,
        rowData = TRUE,
        counts = TRUE,
        return = c("tbl_df", "DataFrameList")
    ) {
        validObject(object)
        assert(
            isFlag(lfcShrink),
            isFlag(rowData),
            isFlag(counts)
        )
        return <- match.arg(return)

        ## Note that this will use the shrunken LFC values, if slotted.
        res <- results(object, results = results, lfcShrink = lfcShrink)

        ## Get the DESeqDataSet, and humanize the sample names.
        ## Note that we're not calling `humanize()` here on the DESeqDataSet,
        ## because we want to keep the gene identifiers in the row names.
        dds <- as(object, "DESeqDataSet")
        ## Always attempt to use human-friendly sample names, defined by the
        ## `sampleName` column in `colData`. We're using this downstream when
        ## joining the normalized counts.
        dds <- convertSampleIDsToNames(dds)

        ## Join the row annotations. DESeq2 includes additional columns in
        ## `rowData()` that aren't informative for a user, and doesn't need to
        ## be included in the tables. Instead, only keep informative columns
        ## that are character or factor. Be sure to drop complex, non-atomic
        ## columns (e.g. list, S4) that are allowed in GRanges/DataFrame but
        ## will fail to write to disk as CSV. Note that we're using `decode()`
        ## here to handle S4 Rle columns from the Genomic Ranges.
        if (isTRUE(rowData)) {
            res <- .joinRowData(results = res, rowData = dds)
        }

        ## Join the normalized counts.
        if (isTRUE(counts)) {
            res <- .joinCounts(results = res, counts = dds)
        }

        ## Using DESeqResults method. Note that join operations above will
        ## coerce from DESeqResults to DataFrame, so we need to coerce back
        ## before `resultsTables()` call.
        assert(is(res, "DESeqResults"))
        validObject(res)

        ## FIXME Eliminate the join steps above...put it in the call here.
        resultsTables(object = res, return = return)
    }



#' @rdname resultsTables
#' @export
setMethod(
    f = "resultsTables",
    signature = signature("DESeqAnalysis"),
    definition = `resultsTables,DESeqAnalysis`
)
