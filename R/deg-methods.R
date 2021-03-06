#' @name deg
#' @inherit AcidGenerics::deg
#' @note Updated 2020-08-05.
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



## Updated 2020-08-05.
`deg,DESeqAnalysis` <-  # nolint
    function(object, i, ...) {
        deg(
            object = results(object, i = i),
            alphaThreshold = alphaThreshold(object),
            lfcThreshold = lfcThreshold(object),
            baseMeanThreshold = baseMeanThreshold(object),
            ...
        )
    }



## Get differential expressed genes (DEGs) from DESeqResults table.
##
## Note that we're not sorting the identifiers here by LFC or P value.
## It's just performing a simple subset to get the identifiers as a character.
##
## Updated 2020-08-04.
`deg,DESeqResults` <-  # nolint
    function(
        object,
        alphaThreshold = NULL,
        lfcThreshold = NULL,
        baseMeanThreshold = NULL,
        direction = c("both", "up", "down"),
        quiet = FALSE
    ) {
        validObject(object)
        if (is.null(alphaThreshold)) {
            alphaThreshold <- alphaThreshold(object)
        }
        if (is.null(lfcThreshold)) {
            lfcThreshold <- lfcThreshold(object)
        }
        if (is.null(baseMeanThreshold)) {
            baseMeanThreshold <- baseMeanThreshold(object)
        }
        assert(
            isAlpha(alphaThreshold),
            isNumber(lfcThreshold),
            isNonNegative(lfcThreshold),
            isNumber(baseMeanThreshold),
            isNonNegative(baseMeanThreshold),
            isFlag(quiet)
        )
        direction <- match.arg(direction)
        data <- as(object, "DataFrame")
        ## Define symbols to use in filtering steps below.
        alphaCol <- "padj"
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
            status <- paste0(status, ")")
            alertInfo(status)
        }
        deg
    }



#' @rdname deg
#' @export
setMethod(
    f = "deg",
    signature = signature("DESeqAnalysis"),
    definition = `deg,DESeqAnalysis`
)



#' @rdname deg
#' @export
setMethod(
    f = "deg",
    signature = signature("DESeqResults"),
    definition = `deg,DESeqResults`
)
