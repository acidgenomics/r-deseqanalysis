# TODO Need to slot DESeqAnalysis package version in object...
# Define a `metadata` list and slot prototype metadata.

# TODO Add a tighter assert check to ensure that `lfcShrink` contains
# shrunken values. Can use `priorInfo` to test for this.



#' DESeq2 differential expression analysis
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
#' or `DESeq2::DESeqDataSetFromMatrix()` constructors to generate the
#' `DESeqDataSet` object.
#'
#' @section DESeqTransform:
#'
#' Object containing variance-stabilized counts. We recommend slotting the
#' return from either [DESeq2::varianceStabilizingTransformation()] or
#' [DESeq2::rlog()].
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
#'
#' @slot transform `DESeqTransform`.
#' @slot results `list`.
#'   One or more unshrunken `DESeqResults`.
#' @slot lfcShrink `list`.
#'   *Optional*. One or more shrunken `DESeqResults`. If set, must correspond to
#'   those defined in `results`.
#'
#' @seealso [DESeqAnalysis()].
#'
#' @return `DESeqAnalysis`.
#'   Contains a `DESeqDataSet`, `DESeqTransform`, and corresponding
#'   `DESeqResults` list.
setClass(
    Class = "DESeqAnalysis",
    slots = c(
        data = "DESeqDataSet",
        transform = "DESeqTransform",
        results = "list",
        lfcShrink = "list"
    ),
    prototype = prototype(
        lfcShrink = list()
    ),
    validity = function(object) {
        data <- slot(object, "data")
        transform <- slot(object, "transform")
        results <- slot(object, "results")
        lfcShrink <- slot(object, "lfcShrink")

        ok <- validate(
            # Require that dimnames are valid.
            hasValidDimnames(data),
            # DESeqDataSet and DESeqTransform must correspond.
            identical(dimnames(data), dimnames(transform)),
            # DESeqDataSet and DESeqResults must correspond.
            all(bapply(
                X = results,
                FUN = function(x) {
                    identical(rownames(x), rownames(data))
                }
            )),
            # DESeqResults list must be named.
            hasNames(results)
        )
        if (!isTRUE(ok)) return(ok)

        # Unshrunken and shrunken DESeqResults must correspond, if shrunken
        # values are optionally defined.
        if (length(lfcShrink) > 0L) {
            ok <- validate(
                all(mapply(
                    unshrunken = results,
                    shrunken = lfcShrink,
                    FUN = function(unshrunken, shrunken) {
                        identical(rownames(unshrunken), rownames(shrunken))
                    },
                    SIMPLIFY = TRUE
                ))
            )
            if (!isTRUE(ok)) return(ok)
        }

        TRUE
    }
)
