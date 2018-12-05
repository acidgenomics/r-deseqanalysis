# FIXME Need to slot DESeqAnalysis package version in object...
# Define a `metadata()` list and slot prototype metadata.

# Consider taking out the `DESeqResultsTables` S4 object. I'm not sure this
# makes sense, and makes the package more complicated...

# TODO Add a tighter assert check to ensure that `lfcShrink` contains
# shrunken values. Can use `priorInfo()` to test for this.

# FIXME Require `results` slot to be named. This makes downstream programming
# easier.



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

        # Require that the DESeqResults list is named.
        valid[["results"]] <- validate_that(has_names(results))

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
