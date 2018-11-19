.valid <- function(list) {
    invalid <- Filter(f = Negate(isTRUE), x = list)
    if (has_length(invalid)) {
        unlist(invalid)
    } else {
        TRUE
    }
}



# DESeqAnalysis ================================================================
#' DESeq2 Analysis Container
#'
#' Class containing all elements generated during differential expression
#' analysis with DESeq2. This class is essentially a `list` with validity checks
#' to ensure `DESeqTransform` and `DESeqResults` correspond to the
#' `DESeqDataSet`.
#'
#' @section DESeqDataSet:
#'
#' We recommend generating the `DESeqDataSet` by coercion from `bcbioRNASeq`
#' object using `as(dds, "bcbioRNASeq")`. Don't use the [DESeq2::DESeqDataSet()]
#' or [DESeq2::DESeqDataSetFromMatrix()] constructors to generate the
#' `DESeqDataSet` object.
#'
#' @section DESeqResults:
#'
#' Don't modify any of the `DESeqResults` objects manually. This includes
#' rearranging the rows or dropping genes without adjusted P values. We'll take
#' care of this automatically in supported methods.
#'
#' @family S4 classes
#' @author Michael Steinbaugh
#' @export
#'
#' @slot data `DESeqDataSet`.
#' @slot transform `DESeqTransform`.
#' @slot results `list`. One or more unshrunken `DESeqResults`.
#' @slot lfcShrink `list`. One or more shrunken `DESeqResults`.
#'
#' @seealso [DESeqAnalysis()].
#' 
#' @return `DESeqAnalysis`, which contains a `DESeqDataSet`, `DESeqTransform`,
#'   and corresponding `DESeqResults`.
setClass(
    Class = "DESeqAnalysis",
    slots = c(
        data = "DESeqDataSet",
        transform = "DESeqTransform",
        results = "list",
        lfcShrink = "list"
    ),
    prototype = list(
        lfcShrink = list()
    )
)
# FIXME Switch to using `validate_that()` here.
setValidity(
    Class = "DESeqAnalysis",
    method = function(object) {
        data <- slot(object, "data")
        transform <- slot(object, "transform")
        results <- slot(object, "results")
        lfcShrink <- slot(object, "lfcShrink")

        assert_that(is(data, "DESeqDataSet"))
        assertHasValidDimnames(data)

        # Require gene-to-symbol mappings.
        assert_is_subset(
            x = c("geneID", "geneName"),
            y = colnames(rowData(data))
        )

        # DESeqDataSet and DESeqTransform must match.
        assert_are_identical(
            x = dimnames(data),
            y = dimnames(transform)
        )

        # DESeqDataSet and DESeqResults must match.
        invisible(lapply(
            X = results,
            FUN = function(x) {
                assert_are_identical(
                    x = rownames(x),
                    y = rownames(data)
                )
            }
        ))

        # DESeqResults and lfcShrink rownames must match, if shrinkage has been
        # calculated.
        if (length(lfcShrink) > 0L) {
            invisible(mapply(
                unshrunken = results,
                shrunken = lfcShrink,
                FUN = function(unshrunken, shrunken) {
                    assert_are_identical(
                        x = rownames(unshrunken),
                        y = rownames(shrunken)
                    )
                }
            ))
        }

        TRUE
    }
)



# DESeqResultsTables ===========================================================
#' DESeq2 Differential Expression Results Tables
#'
#' `DESeqResults` object with `DataFrame` subsets and file path information.
#'
#' @family S4 classes
#' @author Michael Steinbaugh
#' @export
#'
#' @slot results `DESeqResults`. Original unmodified `DESeqResults`. Should
#'   contain all genes, including those with `NA` adjusted *P* values.
#' @slot deg `list`. Differentially expressed genes. Contains `character`
#'   vectors of genes that are upregulated (`up`) or downregulated (`down`).
#'   Values map to the `rownames` of the internal `DESeqResults`. These are
#'   genes that pass `alpha` and `lfcThreshold` cutoffs set in
#'   [DESeq2::results()] call.
#' @slot counts `matrix`. Normalized counts matrix.
#' @slot rowRanges `GRanges`. Row annotations.
#' @slot sampleNames `character`. Human-friendly sample names. Must contain
#'   [names()] that map to the [colnames()] of the `DESeqDataSet`.
#' @slot metadata `list`. Metadata. Contains file paths and information on
#'   whether we're writing locally or to Dropbox.
#'
#' @seealso [DESeqResultsTables()].
#' 
#' @return `DESeqResultsTables`, containing the original, unmodified
#'   `DESeqResults` `DataFrame` along with the corresponding differentially
#'   expressed genes and gene-level metadata (rowRanges).
setClass(
    Class = "DESeqResultsTables",
    slots = c(
        results = "DESeqResults",
        deg = "list",
        counts = "matrix",
        rowRanges = "GRanges",
        sampleNames = "character",
        metadata = "list"
    ),
    prototype = list(
        counts = matrix(),
        rowRanges = GRanges(),
        sampleNames = character(),
        metadata = list()
    )
)
# TODO Add assert check to ensure counts and rowRanges are identical.
setValidity(
    Class = "DESeqResultsTables",
    method = function(object) {
        results <- slot(object, "results")
        assert_that(is(results, "DESeqResults"))
        assert_is_a_string(contrastName(results))

        deg <- slot(object, "deg")
        up <- deg[["up"]]
        assert_is_character(up)
        assert_is_subset(up, rownames(results))
        down <- deg[["down"]]
        assert_is_character(down)
        assert_is_subset(down, rownames(results))
        assert_are_disjoint_sets(up, down)

        alpha <- metadata(results)[["alpha"]]
        assert_is_a_number(alpha)
        lfcThreshold <- metadata(results)[["lfcThreshold"]]
        assert_is_a_number(lfcThreshold)

        # Check that DEGs match the `DESeqResults` summary.
        match <- removeNA(str_match(
            string = capture.output(summary(results)),
            pattern = "^LFC.*\\s\\:\\s([0-9]+).*"
        ))
        assert_are_identical(
            x = length(up),
            y = as.integer(match[1L, 2L])
        )
        assert_are_identical(
            x = length(down),
            y = as.integer(match[2L, 2L])
        )

        TRUE
    }
)
