#' @name markdown
#' @inherit basejump::markdown
#' @inheritParams basejump::params
#' @examples
#' data(deseq)
#'
#' # DESeqAnalysis ====
#' markdown(deseq)
#'
#' ## DESeqResultsTables ====
#' x <- DESeqResultsTables(deseq)
#' markdown(x)
NULL



#' @importFrom basejump markdown
#' @aliases NULL
#' @export
basejump::markdown



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
