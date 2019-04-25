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
#' or [DESeq2::DESeqDataSetFromMatrix()] constructors to generate the
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
#' @slot transform `DESeqTransform`.
#' @slot results `list`.
#'   One or more unshrunken `DESeqResults`.
#' @slot lfcShrink `list`.
#'   *Optional*. One or more shrunken `DESeqResults`. If set, must correspond to
#'   those defined in `results`. Otherwise, can set as empty list (`list()`).
#'
#' @seealso
#' - `help(topic = "Annotated-class", package = "S4Vectors")`.
#' - `showClass("Annotated")`.
setClass(
    Class = "DESeqAnalysis",
    contains = "Annotated",
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
            # DESeqDataSet and DESeqTransform must correspond.
            identical(dimnames(data), dimnames(transform)),
            # results and lfcShrink must be list.
            is.list(results),
            is.list(lfcShrink),
            # DESeqDataSet and DESeqResults must correspond.
            all(bapply(
                X = results,
                FUN = function(x) {
                    identical(rownames(x), rownames(data))
                }
            )),
            # DESeqResults list must be named.
            hasValidNames(results),
            # Require package version in metadata.
            is(metadata(object)[["version"]], "package_version")
        )
        if (!isTRUE(ok)) return(ok)

        # Alpha levels in the slotted results must be identical.
        alphas <- vapply(
            X = results,
            FUN = function(x) {
                metadata(x)[["alpha"]]
            },
            FUN.VALUE = numeric(1L)
        )
        ok <- validate(length(unique(alphas)) == 1L)
        if (!isTRUE(ok)) return(ok)

        # Note that `lfcShrink` slot is currently optional, but that may change
        # in a future update.
        if (hasLength(lfcShrink)) {
            # Unshrunken and shrunken DESeqResults must correspond.
            ok <- validate(
                identical(names(results), names(lfcShrink)),
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

            # Ensure that DESeqResults slotted into `lfcShrink` is actually
            # shrunken using the `lfcShrink()` function. This also checks to
            # ensure that the same method was used for all contrasts.
            shrinkTypes <- vapply(
                X = lfcShrink,
                FUN = lfcShrinkType,
                FUN.VALUE = character(1L)
            )
            ok <- validate(length(unique(shrinkTypes)) == 1L)
            if (!isTRUE(ok)) return(ok)

            # lfcShrink alpha must match the results alpha.
            ok <- validate(
                identical(
                    vapply(
                        X = results,
                        FUN = function(x) {
                            metadata(x)[["alpha"]]
                        },
                        FUN.VALUE = numeric(1L)
                    ),
                    vapply(
                        X = lfcShrink,
                        FUN = function(x) {
                            metadata(x)[["alpha"]]
                        },
                        FUN.VALUE = numeric(1L)
                    )
                )
            )
            if (!isTRUE(ok)) return(ok)
        }

        TRUE
    }
)



#' List containing related DESeq2 analyses
#'
#' @author Michael Steinbaugh
#' @export
setClass(
    Class = "DESeqAnalysisList",
    contains = "SimpleList",
    validity = function(object) {
        # Currently allowing an empty list.
        if (!hasLength(object)) return(TRUE)

        # Require that all objects in list are named.
        ok <- validate(hasValidNames(object))
        if (!isTRUE(ok)) return(ok)

        # Check that all of the elements in the list are DESeqAnalysis.
        ok <- validate(all(bapply(
            X = object,
            FUN = function(object) {
                is(object, "DESeqAnalysis")
            }
        )))
        if (!isTRUE(ok)) return(ok)

        # Ensure that all slotted DESeqAnalysis objects are valid.
        # This step can be slow for large objects.
        ok <- validate(all(bapply(object, validObject)))
        if (!isTRUE(ok)) return(ok)

        TRUE
    }
)
