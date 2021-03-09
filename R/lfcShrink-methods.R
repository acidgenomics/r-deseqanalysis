## FIXME NEED TO RETHINK THIS WITH DESEQRESULTSLIST.
## FIXME NEED TO RETHINK THIS WITH DESEQANALYSISLIST SUPPORT.



#' @name lfcShrink
#' @inherit AcidGenerics::lfcShrink
#' @note Updated 2020-08-04.
#' @seealso [DESeq2::lfcShrink()].
#' @examples
#' data(deseq)
#'
#' ## DESeqAnalysis ====
#' lfcShrink(deseq)
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



## Updated 2020-09-18.
`lfcShrink,DESeqAnalysis` <-  # nolint
    function(object) {
        ok <- metadata(object)[["lfcShrink"]]
        if (is.logical(ok)) return(ok)
        ok <- as.logical(hasLength(slot(object, "lfcShrink")))
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
