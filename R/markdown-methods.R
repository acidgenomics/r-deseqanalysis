#' @name markdown
#' @inherit basejump::markdown
#'
#' @inheritParams basejump::params
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
#' @importFrom bioverbs markdown
#' @usage markdown(object, ...)
#' @export
NULL



markdown.DESeqAnalysis <-  # nolint
    function(object) {
        show(markdownHeader("Contrast names"))
        show(markdownList(.contrastNames(object)))
    }



#' @describeIn markdown List of contrast names.
#' @export
setMethod(
    f = "markdown",
    signature = signature("DESeqAnalysis"),
    definition = markdown.DESeqAnalysis
)
