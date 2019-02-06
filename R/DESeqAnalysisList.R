#' @rdname DESeqAnalysisList-class
#' @param ... `DESeqAnalysis` objects.
#' @export
DESeqAnalysisList <- function(...) {
    datasets <- list(...)
    # Here we're capturing the object names if the user doesn't pass the
    # arguments in as named key value pairs.
    if (is.null(names(datasets))) {
        names <- as.character(match.call(expand.dots = FALSE)$...)
        names(datasets) <- names
    }
    new(Class = "DESeqAnalysisList", datasets)
}
