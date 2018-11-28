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
        
        valid[["dimnames"]] <- validate_that(validDimnames(data))
        # valid[["gene2symbol"]] <- validate_that(
        #     is_subset(
        #         x = c("geneID", "geneName"),
        #         y = colnames(rowData(data))
        #     )
        # )
        
        # Ensure that all objects slotted are matched.
        valid[["matched"]] <- validate_that(
            # DESeqDataSet and DESeqTransform.
            identical(dimnames(data), dimnames(transform)),
            # DESeqDataSet and DESeqResults.
            vapply(
                X = results,
                FUN = function(x) {
                    identical(rownames(x), rownames(data))
                },
                FUN.VALUE = logical(1L)
            )
        )

        # Unshrunken and shrunken DESeqResults.
        if (length(lfcShrink) > 0L) {
            valid[["lfcShrink"]] <- validate_that(
                mapply(
                    unshrunken = results,
                    shrunken = lfcShrink,
                    FUN = function(unshrunken, shrunken) {
                        identical(rownames(unshrunken), rownames(shrunken))
                    },
                    SIMPLIFY = TRUE
                )
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
#' @slot counts `matrix`. Normalized counts matrix.
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
