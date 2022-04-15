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



## Updated 2020-08-04.
`lfcShrinkType,DESeqResults` <- # nolint
    function(object) {
        pi <- priorInfo(object)
        if (isSubset("type", names(pi))) {
            type <- pi[["type"]]
        } else {
            type <- "unshrunken"
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
