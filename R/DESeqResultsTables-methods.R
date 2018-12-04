#' @name DESeqResultsTables
#' @inherit DESeqResultsTables-class
#' @inheritParams basejump::params
#' @inheritParams params
#' @examples
#' data(deseq)
#'
#' ## DESeqAnalysis ====
#' x <- DESeqResultsTables(deseq)
#' print(x)
NULL



DESeqResultsTables.DESeqAnalysis <-  # nolint
    function(
        object,
        results = 1L,
        lfcShrink = TRUE
    ) {
        validObject(object)
        results <- .matchResults(
            object = object,
            results = results,
            lfcShrink = lfcShrink
        )

        # Get the thresholds applied from DESeqResults metadata.
        alpha <- metadata(results)[["alpha"]]
        assertIsAlpha(alpha)
        lfcThreshold <- metadata(results)[["lfcThreshold"]]
        assert_is_a_number(lfcThreshold)
        assert_all_are_non_negative(lfcThreshold)

        # Set LFC and test (P value) columns.
        lfcCol <- "log2FoldChange"
        testCol <- "padj"
        lfc <- sym(lfcCol)
        test <- sym(testCol)
        assert_is_subset(c(lfcCol, testCol), colnames(results))

        # DEG tables are sorted by adjusted P value.
        deg <- results %>%
            as_tibble(rownames = "rowname") %>%
            # Remove genes without an adjusted P value.
            filter(!is.na(!!test)) %>%
            # Remove genes that don't pass alpha cutoff.
            filter(!!test < !!alpha) %>%
            # Arrange by adjusted P value.
            arrange(!!test) %>%
            # Remove genes that don't pass LFC threshold.
            filter(!!lfc > !!lfcThreshold | !!lfc < -UQ(lfcThreshold))

        # Get directional subsets. We'll stash these in the S4 object.
        up <- deg %>%
            filter(!!lfc > 0L) %>%
            pull("rowname")
        down <- deg %>%
            filter(!!lfc < 0L) %>%
            pull("rowname")

        out <- new(
            Class = "DESeqResultsTables",
            results = results,
            deg = list(up = up, down = down)
        )

        # Automatically populate additional slots using DESeqDataSet.
        data <- as(object, "DESeqDataSet")

        # Note that we're slotting the size factor-normalized counts here.
        counts <- counts(data, normalized = TRUE)
        slot(out, "counts") <- counts

        # Slot the row annotations ---------------------------------------------
        # DESeq2 includes additional columns in `mcols()` that aren't
        # informative for a user, and doesn't need to be included in the tables.
        # Instead, only keep informative columns that are character or factor.
        # Be sure to drop complex, non-atomic columns (e.g. list, S4) that are
        # allowed in GRanges/DataFrame but will fail to write to disk as CSV.
        rowRanges <- rowRanges(data)
        mcols <- mcols(rowRanges)
        keep <- vapply(
            X = mcols,
            FUN = function(x) {
                is.character(x) || is.factor(x)
            },
            FUN.VALUE = logical(1L)
        )
        mcols <- mcols[, keep, drop = FALSE]
        mcols(rowRanges) <- mcols
        assert_is_non_empty(rowRanges)
        assert_are_identical(
            x = rownames(data),
            y = names(rowRanges)
        )
        assert_are_disjoint_sets(
            x = colnames(data),
            y = colnames(mcols(rowRanges))
        )
        assert_all_are_true(vapply(
            X = mcols(rowRanges),
            FUN = is.atomic,
            FUN.VALUE = logical(1L)
        ))
        slot(out, "rowRanges") <- rowRanges

        # Slot human-friendly sample names, if they are defined.
        sampleNames <- sampleNames(data)
        if (
            has_length(sampleNames) &&
            !identical(
                x = as.character(sampleNames),
                y = colnames(data)
            )
        ) {
            slot(out, "sampleNames") <- sampleNames
        }

        # Slot metadata.
        slot(out, "metadata") <- list(
            version = metadata(data)[["version"]]
        )

        out
    }



#' @rdname DESeqResultsTables
#' @export
setMethod(
    f = "DESeqResultsTables",
    signature = signature("DESeqAnalysis"),
    definition = DESeqResultsTables.DESeqAnalysis
)
