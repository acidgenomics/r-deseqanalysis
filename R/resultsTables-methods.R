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
#' - `both`: Bidirectional DEGs (up- and down-regulated). This table can be
#'   used for overrepresentation testing but should NOT be used for GSEA.
#'
#' @param DESeqDataSet `DESeqDataSet` or `NULL`.
#' @param extra `logical(1)`.
#'   Include row data and normalized counts from internal `DESeqDataSet`.
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
#' names(x)
#'
#' ## DESeqResults ====
#' ## Use of DESeqAnalysis is encouraged instead of this approach.
#' res <- results(deseq, results = 1L)
#' dds <- as(deseq, "DESeqDataSet")
#' x <- resultsTables(object = res, DESeqDataSet = dds)
#' names(x)
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



## Join the row annotations. DESeq2 includes additional columns in `rowData()`
## that aren't informative for a user, and doesn't need to be included in the
## tables. Instead, only keep informative columns that are character or factor.
## Be sure to drop complex, non-atomic columns (e.g. list, S4) that are allowed
## in GRanges/DataFrame but will fail to write to disk as CSV. Note that we're
## using `decode()` here to handle S4 Rle columns from the Genomic Ranges.
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


## bcbioRNASeq v0.2 release series defaults:
## https://github.com/hbc/bcbioRNASeq/blob/v0.2.10/R/resultsTables-methods.R

## Note that this method is used in bcbioRNASeq F1000 paper.
## Updated 2019-07-23.
`resultsTables,DESeqResults` <-  # nolint
    function(
        object,
        DESeqDataSet = NULL,  # nolint
        alpha = NULL,
        lfcThreshold = NULL,
        return = c("tbl_df", "DataFrameList"),
        ...
    ) {
        validObject(object)
        assert(isAny(DESeqDataSet, c("DESeqDataSet", "NULL")))
        return <- match.arg(return)

        ## Legacy bcbioRNASeq arguments ----------------------------------------
        call <- match.call()
        ## dir
        if (isSubset("dir", names(call))) {
            stop(paste(
                "`dir` argument is defunct.",
                "Use `export()` instead."
            ))
        }
        ## dropboxDir
        if (isSubset("dropboxDir", names(call))) {
            stop(paste(
                "`dropboxDir` argument is defunct.",
                "Use `copyToDropbox()` instead."
            ))
        }
        ## headerLevel
        if (isSubset("headerLevel", names(call))) {
            stop("`headerLevel` argument is defunct.")
        }
        ## rdsToken
        if (isSubset("rdsToken", names(call))) {
            stop(paste(
                "`rdsToken` argument is defunct.",
                "Use `copyToDropbox()` instead."
            ))
        }
        ## summary
        if (isSubset("summary", names(call))) {
            stop("`summary` argument is defunct.")
        }
        ## write
        if (isSubset("write", names(call))) {
            stop(paste(
                "`write` argument is defunct.",
                "Use `export()` instead."
            ))
        }
        ## Check for invalid arguments.
        diff <- setdiff(
            x = setdiff(
                x = names(call),
                y = c("", "...")
            ),
            y = setdiff(
                x = names(formals()),
                y = "..."
            )
        )
        if (hasLength(diff)) {
            stop(sprintf(
                fmt = ngettext(
                    n = length(diff),
                    msg1 = "unused argument (%s)\n",
                    msg2 = "unused arguments (%s)\n"
                ),
                toString(diff)
            ))
        }

        ## Prepare results -----------------------------------------------------
        ## Join row data and counts from DESeqDataSet.
        if (is(DESeqDataSet, "DESeqDataSet")) {
            object <- .joinRowData(results = object, rowData = DESeqDataSet)
            object <- .joinCounts(results = object, counts = DESeqDataSet)
        }
        ## Get the DEG character vectors, which we'll use against the rownames.
        both <- deg(
            object = object,
            alpha = alpha,
            lfcThreshold = lfcThreshold,
            direction = "both"
        )
        ## Early return with warning if there are not DEGs.
        if (!hasLength(both)) {
            warning(paste(
                deparse(results),
                "does not contain any DEGs. Skipping."
            ), call. = FALSE)
            return(invisible())
        }
        up <- deg(
            object = object,
            alpha = alpha,
            lfcThreshold = lfcThreshold,
            direction = "up"
        )
        down <- deg(
            object = object,
            alpha = alpha,
            lfcThreshold = lfcThreshold,
            direction = "down"
        )
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
        extra = TRUE,
        alpha = NULL,
        lfcThreshold = NULL,
        return = c("tbl_df", "DataFrameList")
    ) {
        validObject(object)
        assert(
            isFlag(lfcShrink),
            isFlag(extra)
        )
        return <- match.arg(return)
        ## Note that this will use the shrunken LFC values, if slotted.
        results <- results(
            object = object,
            results = results,
            lfcShrink = lfcShrink
        )
        ## Include extra annotations, if desired.
        if (isTRUE(extra)) {
            ## Get the DESeqDataSet, and humanize the sample names. Note that
            ## we're not calling `humanize()` here on the DESeqDataSet, because
            ## we want to keep the gene identifiers in the row names.
            dds <- as(object, "DESeqDataSet")
            ## Always attempt to use human-friendly sample names, defined by the
            ## `sampleName` column in `colData`. We're using this downstream
            ## when joining the normalized counts.
            dds <- convertSampleIDsToNames(dds)
        } else {
            dds <- NULL
        }
        resultsTables(
            object = results,
            DESeqDataSet = dds,
            alpha = alpha,
            lfcThreshold = lfcThreshold,
            return = return
        )
    }



#' @rdname resultsTables
#' @export
setMethod(
    f = "resultsTables",
    signature = signature("DESeqAnalysis"),
    definition = `resultsTables,DESeqAnalysis`
)
