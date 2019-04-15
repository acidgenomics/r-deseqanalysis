#' @name resultsTables
#' @inherit bioverbs::resultsTables
#' @inheritParams basejump::params
#' @inheritParams params
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
NULL



#' @rdname resultsTables
#' @name resultsTables
#' @importFrom bioverbs resultsTables
#' @export
NULL



# Note that this method is used in bcbioRNASeq F1000 paper.
resultsTables.DESeqResults <-  # nolint
    function(object, return = c("tbl_df", "DataFrameList")) {
        validObject(object)
        return <- match.arg(return)

        # Get the DEG character vectors, which we'll use against the rownames.
        both <- deg(object, direction = "both")
        # Early return with warning if there are not DEGs.
        if (!hasLength(both)) {
            warning(paste(
                deparse(results),
                "does not contain any DEGs. Skipping."
            ), call. = FALSE)
            return(invisible())
        }
        up <- deg(object, direction = "up")
        down <- deg(object, direction = "down")

        # Prepare the return list.
        out <- list(
            all = object,
            up = object[up, , drop = FALSE],
            down = object[down, , drop = FALSE],
            both = object[both, , drop = FALSE]
        )

        # Filter out empty up/down tables.
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
    definition = resultsTables.DESeqResults
)



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

        # Get the DESeqDataSet, and humanize the sample names.
        # Note that we're not calling `humanize()` here on the DESeqDataSet,
        # because we want to keep the gene identifiers in the row names.
        dds <- as(object, "DESeqDataSet")
        # Always attempt to use human-friendly sample names, defined by the
        # `sampleName` column in `colData`. We're using this downstream when
        # joining the normalized counts.
        dds <- convertSampleIDsToNames(dds)

        # Join the row annotations. DESeq2 includes additional columns in
        # `rowData` that aren't informative for a user, and doesn't need to be
        # included in the tables. Instead, only keep informative columns that
        # are character or factor. Be sure to drop complex, non-atomic columns
        # (e.g. list, S4) that are allowed in GRanges/DataFrame but will fail to
        # write to disk as CSV. Note that we're using `decode` here to handle
        # S4 Rle columns from the Genomic Ranges.
        if (isTRUE(rowData)) {
            message("Joining row annotations.")
            rowData <- rowData(dds)
            # SummarizedExperiment inconsistently handles rownames on rowData.
            # Ensure they are set here before continuing.
            rownames(rowData) <- rownames(dds)
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
                identical(rownames(res), rownames(rowData)),
                areDisjointSets(colnames(res), colnames(rowData))
            )
            res <- cbind(res, rowData)
        }

        # Join the normalized counts.
        if (isTRUE(counts)) {
            message("Joining size factor adjusted normalized counts.")
            # We're using the size factor adjusted normalized counts here.
            counts <- counts(dds, normalized = TRUE)
            assert(
                identical(rownames(res), rownames(counts)),
                areDisjointSets(colnames(res), colnames(counts))
            )
            res <- cbind(res, counts)
        }

        # Using DESeqResults method. Note that join operations above will coerce
        # from DESeqResults to DataFrame, so we need to coerce back before
        # `resultsTables()` call.
        res <- as(res, "DESeqResults")
        resultsTables(object = res, return = return)
    }



#' @rdname resultsTables
#' @export
setMethod(
    f = "resultsTables",
    signature = signature("DESeqAnalysis"),
    definition = resultsTables.DESeqAnalysis
)
