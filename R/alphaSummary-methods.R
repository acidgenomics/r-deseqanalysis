#' @name alphaSummary
#' @author Michael Steinbaugh, Lorena Patano
#' @inherit AcidGenerics::alphaSummary
#' @note Updated 2023-09-26.
#'
#' @inheritParams params
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @details
#' Use either `contrast` or `name` to specify the desired contrast.
#'
#' @param alpha `numeric`. Multiple alpha cutoffs.
#'
#' @seealso
#' - `DESeq2::results()`.
#' - `DESeq2::resultsNames()`.
#'
#' @examples
#' data(deseq)
#'
#' ## DESeqAnalysis ====
#' alphaSummary(deseq, contrast = c("condition", "B", "A"))
#' alphaSummary(deseq, name = "condition_B_vs_A")
NULL



## Updated 2020-08-04.
`alphaSummary,DESeqAnalysis` <- # nolint
    function(object, ...) {
        object <- as(object, "DESeqDataSet")
        alphaSummary(object, ...)
    }



## Updated 2023-09-26.
`alphaSummary,DESeqDataSet` <- # nolint
    function(object,
             alpha = c(0.1, 0.05, 0.01, 1e-3, 1e-6),
             contrast = NULL,
             name = NULL) {
        assert(
            validObject(object),
            is.numeric(alpha),
            ## isAlpha requires scalar, so let's apply here.
            all(vapply(
                X = alpha,
                FUN = isAlpha,
                FUN.VALUE = logical(1L)
            )),
            isAny(contrast, classes = c("character", "NULL")),
            isString(name, nullOk = TRUE)
        )
        ## Either `contrast` or `name`.
        ## If neither are defined, we're checking the intercept.
        if (!is.null(contrast) && !is.null(name)) {
            abort(sprintf(
                "Specify either {.arg %s} or {.arg %s}.",
                "contrast", "name"
            ))
        }
        ## Generate an automatic caption.
        if (!is.null(contrast)) {
            caption <- paste(contrast, collapse = " ")
        } else if (!is.null(name)) {
            caption <- name
        } else {
            caption <- resultsNames(object)[[1L]]
        }
        alertInfo(caption)
        data <- vapply(
            X = alpha,
            FUN = function(alpha) {
                args <- list(
                    object = object,
                    contrast = contrast,
                    name = name,
                    alpha = alpha
                )
                args <- Filter(Negate(is.null), args)
                results <- do.call(what = results, args = args)
                ## This requires S3 `summary.DESeqResults` in NAMESPACE.
                ## Currently, DESeq2 isn't exporting this S3 method in BioC
                ## 3.10, so we're pulling the expected S3 method directly
                ## instead. It is called when DESeq2 is attached, but we must
                ## query the method directly when the package is not attached.
                summary <- getS3method(
                    f = "summary",
                    class = "DESeqResults",
                    envir = asNamespace("DESeq2")
                )
                output <- capture.output(summary(results))
                ## Subset the lines of interest from summary.
                ## Keep only the summary lines that contain a colon.
                output <- output[grepl(" : ", output)]
                ## Extract the values after the colon in summary.
                match <- strMatch(
                    x = output,
                    pattern = "^(.+)\\s\\:\\s([[:digit:]]+).*$"
                )
                names <- gsub("\\s+$", "", match[, 2L])
                values <- as.integer(match[, 3L])
                names(values) <- names
                values
            },
            FUN.VALUE = integer(4L)
        )
        colnames(data) <- alpha
        data
    }



#' @rdname alphaSummary
#' @export
setMethod(
    f = "alphaSummary",
    signature = signature(object = "DESeqAnalysis"),
    definition = `alphaSummary,DESeqAnalysis`
)

#' @rdname alphaSummary
#' @export
setMethod(
    f = "alphaSummary",
    signature = signature(object = "DESeqDataSet"),
    definition = `alphaSummary,DESeqDataSet`
)
