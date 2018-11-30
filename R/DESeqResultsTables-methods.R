#' @name DESeqResultsTables
#' @inherit DESeqResultsTables-class
#' @inheritParams basejump::params
#' @inheritParams params
#'
#' @section Obtaining results from DESeq2:
#'
#' It is recommended to specify the `contrast` argument as `character`:
#'
#' 1. Design matrix factor of interest.
#' 2. Numerator for LFC (expt).
#' 3. Denominator for LFC (control).
#'
#' For example, to get the relative change in gene expression between mutant
#' and wildtype samples, here's how to set the contrast vector:
#'
#' ```
#' c(
#'     factor = "genotype",
#'     numerator = "mutant",
#'     denominator = "wildtype"
#' )
#' ```
#'
#' @section Log fold change threshold cutoffs:
#'
#' Refer to `DESeq2::results()` for additional information about using
#' `lfcThreshold` and `altHypothesis` to set an alternative hypothesis based on
#' expected fold changes. In addition, the "Hypothesis tests with thresholds on
#' effect size" section in the DESeq2 paper provides additional explanation.
#'
#' Don't apply *post hoc* LFC threshold filtering to obtain results tables, as
#' this will destroy the meaning of the adjusted *P* values computed. If you are
#' expecting a biological effect size past a particular threshold, set
#' `lfcThreshold`, but be conservative.
#'
#' This [thread][] on the Bioconductor forums explains how `DESeq2::results()`
#' should be called with regard to LFC cutoffs in nice detail.
#'
#' [thread]: https://support.bioconductor.org/p/101504/
#'
#' @seealso
#' - `DESeq2::results()`.
#' - `markdown()`, `write()`.
#'
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
<<<<<<< HEAD
            results = object,
            deg = list(up = up, down = down)
        )
    }



#' @rdname DESeqResultsTables
#' @usage NULL
#' @export
setMethod(
    f = "DESeqResultsTables",
    signature = signature("DESeqResults"),
    definition = DESeqResultsTables.DESeqResults
)



DESeqResultsTables.DESeqAnalysis <-  # nolint
    function(
        object,
        results = 1L,
        lfcShrink = TRUE
    ) {
        validObject(object)

        # Prepare the DESeqResultsTables object with our DESeqResults method.
        results <- .matchResults(
            object = object,
=======
>>>>>>> fea65586f8d60e551a0bbb9c5382bda1999081d3
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
