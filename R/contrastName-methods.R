#' @name contrastName
#' @inherit bioverbs::contrastName
#' @inheritParams basejump::params
#' @inheritParams params
#' @examples
#' data(deseq)
#'
#' ## DESeqResults ====
#' object <- as(deseq, "DESeqResults")
#' contrastName(object)
#'
#' ## DESeqAnalysis ====
#' contrastName(deseq, results = 1L)
NULL



#' @rdname contrastName
#' @name contrastName
#' @importFrom bioverbs contrastName
#' @export
NULL



contrastName.DESeqResults <-  # nolint
    function(object) {
        validObject(object)
        contrast <- mcols(object)[2L, "description"]
        assert(isCharacter(contrast))
        contrast %>%
            gsub("^.*:\\s", "", .) %>%
            gsub("_", " ", .) %>%
            # Improve appearance for difference of differences.
            gsub("\\+", " \\+\n    ", .)
    }



#' @rdname contrastName
#' @export
setMethod(
    f = "contrastName",
    signature = signature("DESeqResults"),
    definition = contrastName.DESeqResults
)



contrastName.DESeqAnalysis <-  # nolint
    function(object, results) {
        do.call(
            what = contrastName,
            args = list(
                object = .matchResults(object, results)
            )
        )
    }



#' @rdname contrastName
#' @export
setMethod(
    f = "contrastName",
    signature = signature("DESeqAnalysis"),
    definition = contrastName.DESeqAnalysis
)
