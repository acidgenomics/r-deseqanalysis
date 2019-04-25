#' @name alphaSummary
#' @author Michael Steinbaugh, Lorena Patano
#' @inherit bioverbs::alphaSummary
#' @inheritParams DESeq2::results
#' @inheritParams basejump::params
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
#' ## DESeqDataSet ====
#' dds <- as(deseq, "DESeqDataSet")
#' design(dds)
#' resultsNames(dds)
#' alphaSummary(dds)
#' alphaSummary(dds, contrast = c("condition", "B", "A"))
#' alphaSummary(dds, name = "condition_B_vs_A")
NULL



alphaSummary.DESeqDataSet <-  # nolint
    function(
        object,
        alpha = c(0.1, 0.05, 0.01, 1e-3, 1e-6),
        contrast = NULL,
        name = NULL
    ) {
        validObject(object)
        assert(
            is.numeric(alpha),
            # isAlpha requires scalar, so let's apply here.
            all(vapply(
                X = alpha,
                FUN = isAlpha,
                FUN.VALUE = logical(1L)
            )),
            isAny(contrast, classes = c("character", "NULL")),
            isString(name, nullOK = TRUE)
        )

        # Either `contrast` or `name`.
        # If neither are defined, we're checking the intercept.
        if (!is.null(contrast) && !is.null(name)) {
            stop("Specify either `contrast` or `name`.", call. = FALSE)
        }

        # Generate an automatic caption.
        if (!is.null(contrast)) {
            caption <- paste(contrast, collapse = " ")
        } else if (!is.null(name)) {
            caption <- name
        } else {
            caption <- resultsNames(object)[[1L]]
        }

        message(paste(
            caption,
            paste("Alpha cutoffs:", toString(alpha)),
            sep = "\n"
        ))

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
                output <- capture.output(summary(results))
                # Subset the lines of interest from summary.
                # Keep only the summary lines that contain a colon.
                output <- output[grepl(" : ", output)]
                # Extract the values after the colon in summary.
                match <- str_match(
                    string = output,
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
    signature = signature("DESeqDataSet"),
    definition = alphaSummary.DESeqDataSet
)
