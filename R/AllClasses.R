## DESeqAnalysis ===============================================================
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
#' object using `as(dds, "bcbioRNASeq")`. Don't use the `DESeq2::DESeqDataSet()`
#' or `DESeq2::DESeqDataSetFromMatrix()` constructors to generate the
#' `DESeqDataSet` object.
#'
#' @section DESeqTransform:
#'
#' Object containing variance-stabilized counts. We recommend slotting the
#' return from either `DESeq2::varianceStabilizingTransformation()` or
#' `DESeq2::rlog()`.
#'
#' @section DESeqResults:
#'
#' Don't modify any of the `DESeqResults` objects manually. This includes
#' rearranging the rows or dropping genes without adjusted P values. We'll take
#' care of this automatically in supported methods.
#'
#' @author Michael Steinbaugh
#' @export
#' @note Updated 2021-03-15.
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
#'
#' @return `DESeqAnalysis`.
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
    )
)
setValidity(
    Class = "DESeqAnalysis",
    method = function(object) {
        data <- slot(object, "data")
        transform <- slot(object, "transform")
        results <- slot(object, "results")
        lfcShrink <- slot(object, "lfcShrink")
        ok <- validate(
            identical(dimnames(data), dimnames(transform)),
            msg = "DESeqDataSet and DESeqTransform must correspond."
        )
        if (!isTRUE(ok)) {
            return(ok)
        }
        ok <- validate(
            is.list(results),
            is.list(lfcShrink),
            msg = "results and lfcShrink must be list."
        )
        if (!isTRUE(ok)) {
            return(ok)
        }
        ok <- validate(
            all(bapply(
                X = results,
                FUN = function(x) {
                    identical(rownames(x), rownames(data))
                }
            )),
            msg = "DESeqDataSet and DESeqResults must correspond."
        )
        if (!isTRUE(ok)) {
            return(ok)
        }
        ok <- validate(
            hasValidNames(results),
            msg = "DESeqResults list must be named."
        )
        if (!isTRUE(ok)) {
            return(ok)
        }
        ## Alpha levels in the slotted results must be identical.
        alphas <- vapply(
            X = results,
            FUN = function(x) {
                metadata(x)[["alpha"]]
            },
            FUN.VALUE = numeric(1L)
        )
        ok <- validate(length(unique(alphas)) == 1L)
        if (!isTRUE(ok)) {
            return(ok)
        }
        ## Note that `lfcShrink` slot is currently optional.
        if (hasLength(lfcShrink)) {
            ok <- validate(
                identical(names(results), names(lfcShrink)),
                all(mapply(
                    unshrunken = results,
                    shrunken = lfcShrink,
                    FUN = function(unshrunken, shrunken) {
                        identical(rownames(unshrunken), rownames(shrunken))
                    },
                    SIMPLIFY = TRUE
                )),
                msg = "Unshrunken and shrunken DESeqResults must correspond."
            )
            if (!isTRUE(ok)) {
                return(ok)
            }
            ## Ensure that DESeqResults slotted into `lfcShrink` is actually
            ## shrunken using the `lfcShrink()` function. This also checks to
            ## ensure that the same method was used for all contrasts.
            shrinkTypes <- vapply(
                X = lfcShrink,
                FUN = lfcShrinkType,
                FUN.VALUE = character(1L)
            )
            ok <- validate(
                length(unique(shrinkTypes)) == 1L,
                msg = "Invalid shrink type."
            )
            if (!isTRUE(ok)) {
                return(ok)
            }
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
                ),
                msg = "lfcShrink alpha must match the results alpha."
            )
            if (!isTRUE(ok)) {
                return(ok)
            }
        }
        TRUE
    }
)



## DESeqAnalysisList ===========================================================
#' List containing related DESeq2 analyses
#'
#' @author Michael Steinbaugh
#' @export
#' @note Updated 2021-03-09.
#' @return `DESeqAnalysisList`.
setClass(
    Class = "DESeqAnalysisList",
    contains = "SimpleList"
)
setValidity(
    Class = "DESeqAnalysisList",
    method = function(object) {
        ## Currently allowing an empty list.
        if (!hasLength(object)) {
            return(TRUE)
        }
        ## Require that all objects in list are named, without duplicates.
        ok <- validate(hasValidNames(object))
        if (!isTRUE(ok)) {
            return(ok)
        }
        ## Check that all of the elements in the list are DESeqAnalysis.
        ok <- validate(
            all(bapply(
                X = object,
                FUN = function(object) {
                    is(object, "DESeqAnalysis")
                }
            )),
            msg = "Not a list of DESeqAnalysis objects."
        )
        if (!isTRUE(ok)) {
            return(ok)
        }
        ## Ensure that all slotted DESeqAnalysis objects are valid.
        ## This step can be slow for large objects.
        ok <- validate(
            all(bapply(object, validObject)),
            msg = "Not all DESeqAnalysis objects are valid."
        )
        if (!isTRUE(ok)) {
            return(ok)
        }
        ## Check that all rownames in slotted DESeqResults are identical.
        rn <- rownames(object[[1L]])
        ok <- validate(
            isCharacter(rn),
            msg = "Row names in first DESeqAnalysis are invalid."
        )
        if (!isTRUE(ok)) {
            return(ok)
        }
        ok <- bapply(
            X = object,
            rn = rn,
            FUN = function(x, rn) {
                identical(rownames(x), rn)
            }
        )
        if (!all(ok)) {
            return(sprintf(
                "Row names mismatch in: %s.",
                toInlineString(names(ok)[!ok], n = 5L)
            ))
        }
        TRUE
    }
)



## DESeqResultsList ============================================================
#' List containing related DESeqResults objects
#'
#' @author Michael Steinbaugh
#' @export
#' @note Updated 2021-03-09.
#' @return `DESeqResultsList`.
setClass(
    Class = "DESeqResultsList",
    contains = "SimpleList"
)
setValidity(
    Class = "DESeqResultsList",
    method = function(object) {
        ## Currently allowing an empty list.
        if (!hasLength(object)) {
            return(TRUE)
        }
        ## Require that all objects in list are named.
        ok <- validate(hasValidNames(object))
        if (!isTRUE(ok)) {
            return(ok)
        }
        ## Check that all of the elements in the list are DESeqAnalysis.
        ok <- validate(
            all(bapply(
                X = object,
                FUN = function(object) {
                    is(object, "DESeqResults")
                }
            )),
            msg = "Not a list of DESeqResults objects."
        )
        if (!isTRUE(ok)) {
            return(ok)
        }
        ## Ensure that all slotted DESeqResults objects are valid.
        ok <- validate(
            all(bapply(object, validObject)),
            msg = "Not all DESeqResults in list are valid."
        )
        if (!isTRUE(ok)) {
            return(ok)
        }
        ## Check that all rownames in slotted DESeqResults are identical.
        rn <- rownames(object[[1L]])
        ok <- validate(
            isCharacter(rn),
            msg = "Row names in first DESeqResults are invalid."
        )
        if (!isTRUE(ok)) {
            return(ok)
        }
        ok <- bapply(
            X = object,
            rn = rn,
            FUN = function(x, rn) {
                identical(rownames(x), rn)
            }
        )
        if (!all(ok)) {
            return(sprintf(
                "Row names mismatch in: %s.",
                toInlineString(names(ok)[!ok], n = 5L)
            ))
        }
        TRUE
    }
)
