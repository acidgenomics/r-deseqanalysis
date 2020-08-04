#' Return shrunken log2 fold change values?
#'
#' @name lfcShrink
#' @note Updated 2020-08-04.
#'
#' @inheritParams acidroxygen::params
#'
#' @return `logical(1)`.
#'
#' @seealso [DESeq2::lfcShrink()].
#'
#' @examples
#' data(deseq)
#' lfcShrink(deseq)
NULL



## Updated 2020-08-04.
`lfcShrink,DESeqDataSet` <-  # nolint
    function(object, ...) {
        validObject(object)
        DESeq2::lfcShrink(object, ...)
    }



#' @rdname lfcShrink
#' @export
setMethod(
    f = "lfcShrink",
    signature = signature("DESeqDataSet"),
    definition = `lfcShrink,DESeqDataSet`
)



## Updated 2020-08-04.
`lfcShrink,DESeqAnalysis` <-  # nolint
    function(object) {
        validObject(object)
        ok <- metadata(object)[["lfcShrink"]]
        if (is.logical(ok)) return(ok)
        ok <- hasLength(slot(object, "lfcShrink"))
        ok
    }



#' @rdname lfcShrink
#' @export
setMethod(
    f = "lfcShrink",
    signature = signature("DESeqAnalysis"),
    definition = `lfcShrink,DESeqAnalysis`
)



## Updated 2020-08-04.
`lfcShrink<-,DESeqAnalysis,logical` <-  # nolint
    function(object, value) {
        assert(isFlag(value))
        metadata(object)[["lfcShrink"]] <- value
        validObject(object)
        object
    }



#' @rdname lfcShrink
#' @export
setReplaceMethod(
    f = "lfcShrink",
    signature = signature(
        object = "DESeqAnalysis",
        value = "logical"
    ),
    definition = `lfcShrink<-,DESeqAnalysis,logical`
)
