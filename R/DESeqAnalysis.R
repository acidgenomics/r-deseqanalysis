#' @rdname DESeqAnalysis-class
#' @export
#'
#' @param data `DESeqDataSet`.
#' @param transform `DESeqTransform`.
#'   [DESeq2::varianceStabilizingTransformation()] recommended by default.
#' @param results `list` or single `DESeqResults`.
#'   One or more unshrunken `DESeqResults`.
#'   Assign the [DESeq2::results()] return here.
#' @param lfcShrink `list`or single `DESeqResults`.
#'   One or more shrunken `DESeqResults`.
#'   Assign the [DESeq2::lfcShrink()] return here.
#'
#' @return `DESeqAnalysis`.
#'   Contains a `DESeqDataSet`, `DESeqTransform`, and corresponding
#'   `DESeqResults` list.
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
        metadata <- list(version = .version)

        # Allow input of single `DESeqResults`.
        if (is(results, "DESeqResults")) {
            results <- .coerceResultsToList(results)
        }
        if (is(lfcShrink, "DESeqResults")) {
            lfcShrink <- .coerceResultsToList(lfcShrink)
        }

        new(
            Class = "DESeqAnalysis",
            data = data,
            transform = transform,
            results = results,
            lfcShrink = lfcShrink,
            metadata = metadata
        )
    }



# Note that this will automatically assign name.
.coerceResultsToList <- function(from) {
    assert(is(from, "DESeqResults"))
    to <- list(from)
    names(to) <- makeNames(contrastName(from))
    to
}



.contrastNames <- function(object) {
    assert(is(object, "DESeqAnalysis"))
    vapply(
        X = slot(object, "results"),
        FUN = contrastName,
        FUN.VALUE = character(1L)
    )
}
