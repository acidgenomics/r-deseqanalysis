#' @inherit DESeqAnalysis-class
#' @export
#'
#' @param data `DESeqDataSet`.
#' @param transform `DESeqTransform`.
#' @param results `list`. One or more unshrunken `DESeqResults`. Assign the
#'   `DESeq2::results()` return here.
#' @param lfcShrink `list`. One or more shrunken `DESeqResults`. Assign the
#'   `DESeq2::lfcShrink()` return here.
#'
#' @examples
#' library(DESeq2)
#' dds <- DESeq(makeExampleDESeqDataSet())
#' class(dds)
#' 
#' dt <- varianceStabilizingTransformation(dds)
#' class(dt)
#' 
#' resultsNames(dds)
#' res <- results(dds, name = resultsNames(dds)[[2L]])
#' class(res)
#' 
#' x <- DESeqAnalysis(
#'     data = dds,
#'     transform = dt,
#'     results = list(res),
#'     lfcShrink = list(lfcShrink(dds = dds, res = res, coef = 2L))
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
    assert_that(is(object, "DESeqAnalysis"))
    vapply(
        X = slot(object, "results"),
        FUN = contrastName,
        FUN.VALUE = character(1L)
    )
}
