# FIXME Add a Filter step here to remove empty.


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
#' @param rowData `logical(1)`. Join the row annotations.
#' @param counts `logical(1)`. Join the size-factor adjusted normalized counts.
#' @param return `character(1)`. Type of data frame to return in the list. Uses
#'   `match.arg`. Note that `DataFrame` option will return with rownames,
#'   whereas `tbl_df` option will return with `"rowname"` column.
#'
#' @return `list`. Named list containing subsets of `DESeqResults`.
#'
#' @examples
#' data(deseq)
#'
#' ## DESeqAnalysis ====
#' x <- resultsTables(deseq, results = 1L)
#' print(x)
NULL



resultsTables.DESeqAnalysis <-  # nolint
    function(
        object,
        results,
        rowData = TRUE,
        counts = TRUE,
        return = c("tbl_df", "DataFrameList")
    ) {
        validObject(object)
        return <- match.arg(return)

        # Note that this will use the shrunken LFC values, if slotted.
        res <- .matchResults(object, results)

        # Get the DESeqDataSet, and humanize the sample na.
        dds <- as(object, "DESeqDataSet")
        # Always attempt to use human-friendly sample names, defined by the
        # `sampleName` column in `colData`. We're using this downstream when
        # joining the normalized counts.
        dds <- convertSampleIDsToNames(dds)

        # Get the DEG character vectors, which we'll use against the rownames.
        both <- deg(res, direction = "both")

        # Early return with warning if there are not DEGs.
        if (!hasLength(both)) {
            warning(paste(
                deparse(results),
                "does not contain any DEGs. Skipping."
            ), call. = FALSE)
            return(invisible())
        }

        up <- deg(res, direction = "up")
        down <- deg(res, direction = "down")

        # Prepare all genes data using S4 DataFrame.
        all <- as(res, "DataFrame")

        # Join the row annotations. DESeq2 includes additional columns in
        # `rowData` that aren't informative for a user, and doesn't need to be
        # included in the tables. Instead, only keep informative columns that
        # are character or factor. Be sure to drop complex, non-atomic columns
        # (e.g. list, S4) that are allowed in GRanges/DataFrame but will fail to
        # write to disk as CSV. Note that we're using `decode` here to handle
        # S4 Rle columns from the Genomic Ranges.
        if (isTRUE(rowData)) {
            message("Joining row annotations.")
            rowData <- decode(rowData(dds))
            assert(hasRownames(rowData))
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
                identical(rownames(all), rownames(rowData)),
                areDisjointSets(colnames(all), colnames(rowData))
            )
            all <- cbind(all, rowData)
        }

        # Join the normalized counts.
        if (isTRUE(counts)) {
            message("Joining size factor adjusted normalized counts.")
            # We're using the size factor adjusted normalized counts here.
            counts <- counts(dds, normalized = TRUE)
            assert(
                identical(rownames(all), rownames(counts)),
                areDisjointSets(colnames(all), colnames(counts))
            )
            all <- cbind(all, counts)
        }

        # Prepare the return list.
        out <- list(
            all = all,
            up = all[up, , drop = FALSE],
            down = all[down, , drop = FALSE],
            both = all[both, , drop = FALSE]
        )

        # Filter out empty up/down tibbles.
        out <- Filter(hasRows, out)

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
    signature = signature("DESeqAnalysis"),
    definition = resultsTables.DESeqAnalysis
)
