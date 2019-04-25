#' Shrunken log2 fold change (LFC) type
#'
#' @name lfcShrinkType
#' @inheritParams params
#'
#' @seealso [DESeq2::lfcShrink()].
NULL



lfcShrinkType.DESeqResults <-  # nolint
    function(object) {
        validObject(object)
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
    definition = lfcShrinkType.DESeqResults
)



lfcShrinkType.DESeqAnalysis <-  # nolint
    function(object) {
        validObject(object)
        lfcShrink <- slot(object, "lfcShrink")
        if (!hasLength(lfcShrink)) return(NULL)
        lfcShrinkType(lfcShrink[[1L]])
    }



#' @rdname lfcShrinkType
#' @export
setMethod(
    f = "lfcShrinkType",
    signature = signature("DESeqAnalysis"),
    definition = lfcShrinkType.DESeqAnalysis
)
