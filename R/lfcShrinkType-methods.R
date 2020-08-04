#' @name lfcShrinkType
#' @inherit acidgenerics::lfcShrinkType
#' @note Updated 2020-08-04.
#' @param ... Additional arguments.
#' @seealso [DESeq2::lfcShrink()].
#' @examples
#' data(deseq)
#'
#' ## DESeqAnalysis ====
#' lfcShrinkType(deseq)
NULL



#' @rdname lfcShrinkType
#' @name lfcShrinkType
#' @importFrom acidgenerics lfcShrinkType
#' @usage lfcShrinkType(object, ...)
#' @export
NULL



## Updated 2019-07-23.
`lfcShrinkType,DESeqResults` <-  # nolint
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
    definition = `lfcShrinkType,DESeqResults`
)



## Updated 2020-08-04.
`lfcShrinkType,DESeqAnalysis` <-  # nolint
    function(object) {
        validObject(object)
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
