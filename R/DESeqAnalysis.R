#' @inherit DESeqAnalysis-class
#' @export
#'
#' @param data `DESeqDataSet`.
#' @param transform `DESeqTransform`.
#' @param results `list`. One or more unshrunken `DESeqResults`. Assign the
#'   `DESeq2::results` return here.
#' @param lfcShrink `list`. One or more shrunken `DESeqResults`. Assign the
#'   `DESeq2::lfcShrink` return here.
#'
#' @examples
#' library(DESeq2)
#'
#' data <- DESeq(makeExampleDESeqDataSet())
#' class(data)
#'
#' transform <- varianceStabilizingTransformation(data)
#' class(transform)
#'
#' resultsNames(data)
#' name <- resultsNames(data)[[2L]]
#' results <- results(data, name = name)
#' class(results)
#'
#' lfcShrink <- lfcShrink(dds = data, res = results, coef = 2L)
#'
#' results <- list(results)
#' names(results) <- name
#' lfcShrink <- list(lfcShrink)
#' names(lfcShrink) <- name
#' identical(names(results), names(lfcShrink))
#'
#' x <- DESeqAnalysis(
#'     data = data,
#'     transform = transform,
#'     results = results,
#'     lfcShrink = lfcShrink
#' )
#' print(x)
DESeqAnalysis <-  # nolint
    function(
        data,
        transform,
        results,
        lfcShrink
    ) {
        new(
            Class = "DESeqAnalysis",
            data = data,
            transform = transform,
            results = results,
            lfcShrink = lfcShrink
        )
    }



.contrastNames <- function(object) {
    assert(is(object, "DESeqAnalysis"))
    vapply(
        X = slot(object, "results"),
        FUN = contrastName,
        FUN.VALUE = character(1L)
    )
}
