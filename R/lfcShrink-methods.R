#' @name lfcShrink
#' @inherit acidgenerics::lfcShrink
#' @note Updated 2020-08-04.
#' @seealso [DESeq2::lfcShrink()].
#' @examples
#' data(deseq)
#'
#' ## DESeqAnalysis ====
#' lfcShrink(deseq)
NULL



#' @rdname lfcShrink
#' @name lfcShrink
#' @importFrom acidgenerics lfcShrink
#' @usage lfcShrink(object, ...)
#' @export
NULL

#' @rdname lfcShrink
#' @name lfcShrink<-
#' @importFrom acidgenerics lfcShrink<-
#' @usage lfcShrink(object, ...) <- value
#' @export
NULL



## Updated 2020-08-04.
`lfcShrink,DESeqDataSet` <-  # nolint
    function(object, ...) {
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
