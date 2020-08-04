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
