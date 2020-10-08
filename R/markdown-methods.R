#' @name markdown
#' @inherit basejump::markdown
#' @note Updated 2019-09-10.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @examples
#' data(deseq)
#'
#' # DESeqAnalysis ====
#' markdown(deseq)
NULL



#' @rdname markdown
#' @name markdown
#' @importFrom AcidGenerics markdown
#' @usage markdown(object, ...)
#' @export
NULL



## Updated 2019-09-10.
`markdown,DESeqAnalysis` <-  # nolint
    function(object) {
        show(markdownHeader("Contrast names"))
        show(markdownList(contrastNames(object)))
    }



#' @describeIn markdown List of contrast names.
#' @export
setMethod(
    f = "markdown",
    signature = signature("DESeqAnalysis"),
    definition = `markdown,DESeqAnalysis`
)
