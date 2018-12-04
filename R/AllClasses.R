# FIXME Need to slot DESeqAnalysis package version in object...
# Define a `metadata()` list and slot prototype metadata.

# Consider taking out the `DESeqResultsTables` S4 object. I'm not sure this
# makes sense, and makes the package more complicated...



validateS4 <- function(...) {
    list <- list(...)
    if (is.list(list[[1L]])) {
        list <- list[[1L]]
    }
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
#' object using `as(dds, "bcbioRNASeq")`. Don't use the `DESeq2::DESeqDataSet()`
#' or `DESeq2::DESeqDataSetFromMatrix()` constructors to generate the
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
#' @seealso `DESeqAnalysis()`.
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
setValidity(
    Class = "DESeqAnalysis",
    method = function(object) {
        valid <- list()

        data <- slot(object, "data")
        transform <- slot(object, "transform")
        results <- slot(object, "results")
        lfcShrink <- slot(object, "lfcShrink")

        # Require that dimnames are valid.
        valid[["dimnames"]] <- validate_that(validDimnames(data))

        # Ensure that all objects slotted are matched.
        valid[["matched"]] <- validate_that(
            # DESeqDataSet and DESeqTransform.
            identical(dimnames(data), dimnames(transform)),
            # DESeqDataSet and DESeqResults.
            all(vapply(
                X = results,
                FUN = function(x) {
                    identical(rownames(x), rownames(data))
                },
                FUN.VALUE = logical(1L)
            ))
        )

        # Unshrunken and shrunken DESeqResults.
        if (length(lfcShrink) > 0L) {
            valid[["lfcShrink"]] <- validate_that(
                all(mapply(
                    unshrunken = results,
                    shrunken = lfcShrink,
                    FUN = function(unshrunken, shrunken) {
                        identical(rownames(unshrunken), rownames(shrunken))
                    },
                    SIMPLIFY = TRUE
                ))
            )
        }

        validateS4(valid)
    }
)



# DESeqResultsTables ===========================================================
#' DESeq2 Differential Expression Results Tables
#'
#' `DESeqResults` object with `DataFrame` subsets and file path information.
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
#'   `DESeq2::results()` call.
#' @slot counts `matrix`. Size-factor adjusted (normalized) counts matrix from
#'   the `DESeqDataSet`. This matrix is returned by `DESeq2::counts(object,
#'   normalized = TRUE)`. These should NOT be the log2 variance-stabilized
#'   counts defined in the `DESeqTransform` object.
#' @slot rowRanges `GRanges`. Row annotations.
#' @slot sampleNames `character`. Human-friendly sample names. Must contain
#'   `names()` that map to the `colnames()` of the `DESeqDataSet`.
#' @slot metadata `list`. Metadata. Contains file paths and information on
#'   whether we're writing locally or to Dropbox.
#'
#' @seealso `DESeqResultsTables()`.
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
setValidity(
    Class = "DESeqResultsTables",
    method = function(object) {
        valid <- list()

        results <- slot(object, "results")
        alpha <- metadata(results)[["alpha"]]
        lfcThreshold <- metadata(results)[["lfcThreshold"]]

        deg <- slot(object, "deg")
        up <- deg[["up"]]
        down <- deg[["down"]]

        # Check that DEGs match the `DESeqResults` summary.
        degMatch <- removeNA(str_match(
            string = capture.output(summary(results)),
            pattern = "^LFC.*\\s\\:\\s([0-9]+).*"
        ))

        validate_that(
            is(results, "DESeqResults"),
            is.character(up),
            is_subset(up, rownames(results)),
            is.character(down),
            is_subset(down, rownames(results)),
            are_disjoint_sets(up, down),
            is_a_number(alpha),
            is_a_number(lfcThreshold),
            identical(
                x = length(up),
                y = as.integer(degMatch[1L, 2L])
            ),
            identical(
                x = length(down),
                y = as.integer(degMatch[2L, 2L])
            )
        )
    }
)
