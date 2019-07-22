## How to get names of dot arguments.
## https://stackoverflow.com/questions/51259346


#' @rdname DESeqAnalysisList-class
#' @param ... `DESeqAnalysis` objects.
#' @export
DESeqAnalysisList <- function(...) {  # nolint
    mc <- match.call(expand.dots = FALSE)
    dots <- list(...)
    dotsNames <- as.character(mc[["..."]])

    ## Look to see if the user passed in a list.
    if (
        hasLength(dots, n = 1L) &&
        is.list(dots[[1L]])
    ) {
        data <- dots[[1L]]
    } else {
        data <- dots
        ## Here we're capturing the object names if the user doesn't pass the
        ## arguments in as named key value pairs.
        if (is.null(names(data))) {
            names(data) <- dotsNames
        }
    }

    new(Class = "DESeqAnalysisList", data)
}
