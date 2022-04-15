#' @name lfcShrink
#' @inherit AcidGenerics::lfcShrink
#' @note Updated 2021-06-28.
#'
#' @seealso `DESeq2::lfcShrink()`.
#'
#' @examples
#' data(deseq)
#'
#' ## DESeqAnalysis ====
#' lfcShrink(deseq)
NULL



## Updated 2020-08-04.
`lfcShrink,DESeqDataSet` <- # nolint
    function(object, ...) {
        DESeq2::lfcShrink(object, ...)
    }



## Updated 2020-09-18.
`lfcShrink,DESeqAnalysis` <- # nolint
    function(object) {
        ok <- metadata(object)[["lfcShrink"]]
        if (is.logical(ok)) {
            return(ok)
        }
        ok <- as.logical(hasLength(slot(object, "lfcShrink")))
        ok
    }



## Updated 2021-03-12.
`lfcShrink,DESeqAnalysisList` <- # nolint
    function(object) {
        assert(hasLength(object))
        lfcShrink(object[[1L]])
    }



## Updated 2020-08-04.
`lfcShrink<-,DESeqAnalysis,logical` <- # nolint
    function(object, value) {
        assert(isFlag(value))
        metadata(object)[["lfcShrink"]] <- value
        object
    }



## Updated 2020-08-04.
`lfcShrink<-,DESeqAnalysisList,logical` <- # nolint
    function(object, value) {
        assert(
            hasLength(object),
            isFlag(value)
        )
        metadata(object[[1L]])[["lfcShrink"]] <- value
        object
    }



#' @rdname lfcShrink
#' @export
setMethod(
    f = "lfcShrink",
    signature = signature(object = "DESeqAnalysis"),
    definition = `lfcShrink,DESeqAnalysis`
)

#' @rdname lfcShrink
#' @export
setMethod(
    f = "lfcShrink",
    signature = signature(object = "DESeqAnalysisList"),
    definition = `lfcShrink,DESeqAnalysisList`
)

#' @rdname lfcShrink
#' @export
setMethod(
    f = "lfcShrink",
    signature = signature(object = "DESeqDataSet"),
    definition = `lfcShrink,DESeqDataSet`
)



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

#' @rdname lfcShrink
#' @export
setReplaceMethod(
    f = "lfcShrink",
    signature = signature(
        object = "DESeqAnalysisList",
        value = "logical"
    ),
    definition = `lfcShrink<-,DESeqAnalysisList,logical`
)
