#' Differential Expression Results Tables
#'
#' Generate tables summarizing the differential expression, with subsets for
#' differentially expressed genes (DEGs).
#'
#' DEG tables (i.e. everything except the `all` table), are arranged by adjusted
#' *P* value.
#'
#' @note Do not apply post hoc log fold change cutoffs.
#'
#' @section Tables:
#'
#' - `all`: All genes, including genes without an adjusted *P* value. This table
#' is unmodified, and the rows have not been re-arranged or subset. It is
#' suitable for gene set enrichment analysis (GSEA).
#' - `up`: Upregulated genes.
#' - `down`: Downregulated genes.
#' - `both`: Bi-directional DEGs (up- and down-regulated). This table can be
#'   used for overrepresentation testing but should NOT be used for GSEA.
#'
#' @name resultsTables
#' @inheritParams basejump::params
#' @inheritParams params
#'
#' @param return `string`. Type of data frame to return in the list. Uses
#'   `match.arg()`. Note that `DataFrame` option will return with rownames,
#'   whereas `tbl_df` option will return with `"rowname"` column.
#'
#' @return `list`. Named list containing subsets of `DESeqResults`.
#'
#' @examples
#' data(deseq)
#'
#' ## DESeqAnalysis ====
#' x <- resultsTables(deseq)
#' print(x)
NULL



resultsTables.DESeqAnalysis <-  # nolint
    function(
        object,
        results = 1L,
        return = c("DataFrame", "tbl_df")
    ) {
        validObject(object)
        return <- match.arg(return)

        # Note that this will use the shrunken LFC values, if slotted.
        res <- .matchResults(object, results)

        # Get the DESeqDataSet, and humanize the sample na.
        dds <- as(object, "DESeqDataSet")
        # Always attempt to use human-friendly sample names, defined by the
        # `sampleName` column in `colData()`. We're using this downstream when
        # joining the normalized counts.
        dds <- convertSampleIDsToNames(dds)

        # We're using the size factor adjusted normalized counts here.
        counts <- counts(dds, normalized = TRUE)

        # Get the DEG character vectors, which we'll use against the rownames.
        up <- deg(object, direction = "up")
        down <- deg(object, direction = "down")
        both <- deg(object, direction = "both")

        # Prepare all genes data using S4 DataFrame.
        all <- as(res, "DataFrame")

        # Join the row annotations. DESeq2 includes additional columns in
        # `rowData()` that aren't informative for a user, and doesn't need to be
        # included in the tables. Instead, only keep informative columns that
        # are character or factor. Be sure to drop complex, non-atomic columns
        # (e.g. list, S4) that are allowed in GRanges/DataFrame but will fail to
        # write to disk as CSV. Note that we're using `decode()` here to handle
        # S4 Rle columns from the Genomic Ranges.
        rowData <- decode(rowData(dds))
        assertHasRownames(rowData)
        keep <- vapply(
            X = rowData,
            FUN = function(x) {
                is.character(x) || is.factor(x)
            },
            FUN.VALUE = logical(1L)
        )
        rowData <- rowData[, keep, drop = FALSE]
        assert_all_are_true(vapply(
            X = rowData,
            FUN = is.atomic,
            FUN.VALUE = logical(1L)
        ))
        assert_is_non_empty(rowData)
        assert_are_identical(rownames(all), rownames(rowData))
        assert_are_disjoint_sets(colnames(all), colnames(rowData))
        all <- cbind(all, rowData)

        # Join the normalized counts.
        assert_are_identical(rownames(all), rownames(counts))
        assert_are_disjoint_sets(colnames(all), colnames(counts))
        all <- cbind(all, counts)

        # Prepare the return list.
        out <- list(
            all = all,
            up = all[up, , drop = FALSE],
            down = all[down, , drop = FALSE],
            both = all[both, , drop = FALSE]
        )

        switch(
            EXPR = return,
            DataFrame = out,
            tbl_df = lapply(out, as_tibble)
        )
    }



#' @rdname resultsTables
#' @export
setMethod(
    f = "resultsTables",
    signature = signature("DESeqAnalysis"),
    definition = resultsTables.DESeqAnalysis
)
