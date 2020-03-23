#' @name deg
#' @inherit acidgenerics::deg
#' @note Updated 2019-12-18.
#'
#' @inheritParams acidroxygen::params
#' @inheritParams params
#' @param ... Additional arguments.
#'
#' @examples
#' data(deseq)
#'
#' ## DESeqAnalysis ====
#' x <- deg(deseq, i = 1L)
#' head(x)
NULL



#' @rdname deg
#' @name deg
#' @importFrom acidgenerics deg
#' @usage deg(object, ...)
#' @export
NULL



## Get differential expressed genes (DEGs) from DESeqResults table.
##
## Note that we're not sorting the identifiers here by LFC or P value.
## It's just performing a simple subset to get the identifiers as a character.
##
## Updated 2019-08-20.
`deg,DESeqResults` <-  # nolint
    function(
        object,
        alpha = NULL,
        lfcThreshold = NULL,
        direction = c("both", "up", "down")
    ) {
        validObject(object)
        if (is.null(alpha)) {
            alpha <- metadata(object)[["alpha"]]
        }
        assert(isAlpha(alpha))
        if (is.null(lfcThreshold)) {
            lfcThreshold <- metadata(object)[["lfcThreshold"]]
        }
        assert(
            isNumber(lfcThreshold),
            isNonNegative(lfcThreshold)
        )
        direction <- match.arg(direction)
        data <- as(object, "DataFrame")
        ## Define symbols to use in filtering steps below.
        alphaCol <- "padj"
        lfcCol <- "log2FoldChange"
        data <- data[, c(lfcCol, alphaCol)]
        ## Apply alpha cutoff.
        keep <- which(data[[alphaCol]] < alpha)
        data <- data[keep, , drop = FALSE]
        ## Apply LFC threshold cutoff.
        if (lfcThreshold > 0L) {
            keep <- which(abs(data[[lfcCol]]) > lfcThreshold)
            data <- data[keep, , drop = FALSE]
        }
        ## Apply directional filtering.
        if (identical(direction, "up")) {
            keep <- which(data[[lfcCol]] > 0L)
            data <- data[keep, , drop = FALSE]
        } else if (identical(direction, "down")) {
            keep <- which(data[[lfcCol]] < 0L)
            data <- data[keep, , drop = FALSE]
        }
        ## Arrange table by adjusted P value.
        data <- data[order(data[[alphaCol]]), , drop = FALSE]
        deg <- rownames(data)
        status <- sprintf(
            fmt = "%d %s %s detected (alpha: %g; lfc: %g).",
            length(deg),
            switch(
                EXPR = direction,
                up = "upregulated",
                down = "downregulated",
                both = "differentially expressed"
            ),
            ngettext(
                n = length(deg),
                msg1 = "gene",
                msg2 = "genes"
            ),
            alpha,
            lfcThreshold
        )
        cli_alert_info(status)
        deg
    }



#' @rdname deg
#' @export
setMethod(
    f = "deg",
    signature = signature("DESeqResults"),
    definition = `deg,DESeqResults`
)



## Updated 2019-12-18.
`deg,DESeqAnalysis` <-  # nolint
    function(
        object,
        i,
        ...
    ) {
        ## nocov start
        call <- match.call()
        ## results
        if ("results" %in% names(call)) {
            stop("'results' is defunct in favor of 'i'.")
        }
        rm(call)
        ## nocov end
        res <- results(object = object, i = i, lfcShrink = FALSE)
        deg(object = res, ...)
    }



#' @rdname deg
#' @export
setMethod(
    f = "deg",
    signature = signature("DESeqAnalysis"),
    definition = `deg,DESeqAnalysis`
)
