#' @name lfcShrinkType
#' @inherit AcidGenerics::lfcShrinkType
#' @note Updated 2021-03-12.
#'
#' @param ... Additional arguments.
#'
#' @seealso `DESeq2::lfcShrink()`.
#'
#' @examples
#' data(deseq)
#'
#' ## DESeqAnalysis ====
#' lfcShrinkType(deseq)
NULL



## Updated 2021-03-12.
`lfcShrinkType,DESeqAnalysis` <- # nolint
    function(object) {
        if (!isTRUE(lfcShrink(object))) {
            return(NULL)
        }
        res <- results(object, i = 1L, quiet = TRUE)
        lfcShrinkType(res)
    }



## Updated 2021-03-12.
`lfcShrinkType,DESeqAnalysisList` <- # nolint
    function(object) {
        assert(hasLength(object))
        lfcShrinkType(object[[1L]])
    }



#' Log fold change shrinkage type
#'
#' @note Updated 2023-12-18.
#' @noRd
#'
#' @seealso
#' - https://doi.org/10.1186/s13059-014-0550-8
`lfcShrinkType,DESeqResults` <- # nolint
    function(object) {
        pi <- try(
            expr = {
                priorInfo(object)
            },
            silent = TRUE
        )
        ## This provides support for legacy DESeqResults objects generated
        ## before to `priorInfo` support.
        if (is(pi, "try-error")) {
            mc <- mcols(object)
            desc <- mc["log2FoldChange", "description"]
            ## MAP (shrunken) vs. MLE (unshrunken).
            if (isMatchingFixed(x = desc, pattern = "MAP")) {
                type <- "shrunken"
            } else if (isMatchingFixed(x = desc, pattern = "MLE")) {
                type <- "unshrunken"
            } else {
                type <- "unknown"
            }
        } else {
            if (isSubset("type", names(pi))) {
                type <- pi[["type"]]
            } else {
                type <- "unshrunken"
            }
        }
        assert(isString(type))
        type
    }



## Updated 2021-03-12.
`lfcShrinkType,DESeqResultsList` <- # nolint
    function(object) {
        assert(hasLength(object))
        lfcShrinkType(object[[1L]])
    }



#' @rdname lfcShrinkType
#' @export
setMethod(
    f = "lfcShrinkType",
    signature = signature(object = "DESeqAnalysis"),
    definition = `lfcShrinkType,DESeqAnalysis`
)

#' @rdname lfcShrinkType
#' @export
setMethod(
    f = "lfcShrinkType",
    signature = signature(object = "DESeqAnalysisList"),
    definition = `lfcShrinkType,DESeqAnalysisList`
)

#' @rdname lfcShrinkType
#' @export
setMethod(
    f = "lfcShrinkType",
    signature = signature(object = "DESeqResults"),
    definition = `lfcShrinkType,DESeqResults`
)

#' @rdname lfcShrinkType
#' @export
setMethod(
    f = "lfcShrinkType",
    signature = signature(object = "DESeqResultsList"),
    definition = `lfcShrinkType,DESeqResultsList`
)
