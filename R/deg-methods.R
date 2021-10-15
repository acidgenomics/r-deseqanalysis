#' @name deg
#' @inherit AcidGenerics::deg
#' @note Updated 2020-08-09.
#'
#' @inheritParams AcidRoxygen::params
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



## Updated 2021-06-29.
`deg,DESeqAnalysis` <-  # nolint
    function(object, i, ...) {
        deg(
            object = results(object, i = i),
            alphaThreshold = alphaThreshold(object),
            baseMeanThreshold = baseMeanThreshold(object),
            lfcThreshold = lfcThreshold(object),
            ...
        )
    }



## Get differential expressed genes (DEGs) from DESeqResults table.
##
## Note that we're not sorting the identifiers here by LFC or P value.
## It's just performing a simple subset to get the identifiers as a character.
##
## Updated 2021-08-09.
`deg,DESeqResults` <-  # nolint
    function(
        object,
        direction = c("both", "up", "down"),
        alphaThreshold = NULL,
        baseMeanThreshold = NULL,
        lfcThreshold = NULL,
        quiet = FALSE
    ) {
        validObject(object)
        if (is.null(alphaThreshold)) {
            alphaThreshold <- alphaThreshold(object)
        }
        if (is.null(baseMeanThreshold)) {
            baseMeanThreshold <- baseMeanThreshold(object)
        }
        if (is.null(lfcThreshold)) {
            lfcThreshold <- lfcThreshold(object)
        }
        assert(
            hasRownames(object),
            isAlpha(alphaThreshold),
            isNumber(baseMeanThreshold),
            isNonNegative(baseMeanThreshold),
            isNumber(lfcThreshold),
            isNonNegative(lfcThreshold),
            isFlag(quiet)
        )
        direction <- match.arg(direction)
        data <- as(object, "DataFrame")
        ## Define symbols to use in filtering steps below.
        alphaCol <- .alphaCol(object)
        lfcCol <- "log2FoldChange"
        baseMeanCol <- "baseMean"
        cols <- c(alphaCol, baseMeanCol, lfcCol)
        assert(isSubset(cols, colnames(data)))
        data <- data[, cols, drop = FALSE]
        ## Apply alpha cutoff.
        keep <- which(data[[alphaCol]] < alphaThreshold)
        data <- data[keep, , drop = FALSE]
        ## Apply LFC threshold cutoff.
        if (lfcThreshold > 0L) {
            keep <- which(abs(data[[lfcCol]]) >= lfcThreshold)
            data <- data[keep, , drop = FALSE]
        }
        ## Apply base mean cutoff.
        if (baseMeanThreshold > 0L) {
            keep <- which(data[[baseMeanCol]] >= baseMeanThreshold)
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
        if (!isTRUE(quiet)) {
            sep <- "; "
            status <- sprintf(
                fmt = "%d %s %s",
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
                )
            )
            status <- paste0(status, " (alpha < ", alphaThreshold)
            if (lfcThreshold > 0L) {
                status <- paste0(
                    status, sep,
                    "lfc >= ", lfcThreshold
                )
            }
            if (baseMeanThreshold > 0L) {
                status <- paste0(
                    status, sep,
                    "baseMean >= ", baseMeanThreshold
                )
            }
            status <- paste0(status, ").")
            alertInfo(status)
        }
        deg
    }



#' @rdname deg
#' @export
setMethod(
    f = "deg",
    signature = signature(object = "DESeqAnalysis"),
    definition = `deg,DESeqAnalysis`
)

#' @rdname deg
#' @export
setMethod(
    f = "deg",
    signature = signature(object = "DESeqResults"),
    definition = `deg,DESeqResults`
)
