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



markdown.DESeqResultsTables <-  # nolint
    function(object, headerLevel = 2L) {
        assert_that(is(object, "DESeqResultsTables"))
        validObject(object)
        assertIsHeaderLevel(headerLevel)

        metadata <- slot(object, "metadata")

        # Include a contrast header, which is useful for looping.
        contrast <- contrastName(object)
        markdownHeader(contrast, level = headerLevel, asis = TRUE)
        headerLevel <- headerLevel + 1L

        # Top tables.
        markdownHeader("Top tables", level = headerLevel, asis = TRUE)
        topTables(object)
    }



#' @describeIn markdown File paths (if exported) and top tables.
#' @export
setMethod(
    f = "markdown",
    signature = signature("DESeqResultsTables"),
    definition = markdown.DESeqResultsTables
)
