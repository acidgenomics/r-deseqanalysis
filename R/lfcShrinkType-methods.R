## FIXME NEED TO RETHINK THIS WITH DESEQRESULTSLIST.



#' @name lfcShrinkType
#' @inherit AcidGenerics::lfcShrinkType
#' @note Updated 2020-08-04.
#' @param ... Additional arguments.
#' @seealso [DESeq2::lfcShrink()].
#' @examples
#' data(deseq)
#'
#' ## DESeqAnalysis ====
#' lfcShrinkType(deseq)
NULL



## Updated 2020-08-04.
`lfcShrinkType,DESeqResults` <-  # nolint
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



#' @rdname lfcShrinkType
#' @export
setMethod(
    f = "lfcShrinkType",
    signature = signature("DESeqResults"),
    definition = `lfcShrinkType,DESeqResults`
)



## Updated 2020-08-04.
`lfcShrinkType,DESeqAnalysis` <-  # nolint
    function(object) {
        if (!isTRUE(lfcShrink(object))) return(NULL)
        lfcShrinkType(slot(object, "lfcShrink")[[1L]])
    }



#' @rdname lfcShrinkType
#' @export
setMethod(
    f = "lfcShrinkType",
    signature = signature("DESeqAnalysis"),
    definition = `lfcShrinkType,DESeqAnalysis`
)
