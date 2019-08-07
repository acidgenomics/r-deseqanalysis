#' @name deg
#' @inherit bioverbs::deg
#'
#' @inheritParams acidroxygen::params
#' @inheritParams params
#' @param ... Additional arguments.
#'
#' @examples
#' data(deseq)
#'
#' ## DESeqAnalysis ====
#' x <- deg(deseq, results = 1L)
#' head(x)
NULL



#' @rdname deg
#' @name deg
#' @importFrom bioverbs deg
#' @usage deg(object, ...)
#' @export
NULL



## Get differential expressed genes (DEGs) from DESeqResults table.
## Note that we're not sorting the identifiers here by LFC or P value.
## It's just performing a simple subset to get the identifiers as a character.
## Updated 2019-07-23.
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

        ## Define symbols to use in dplyr calls below.
        alphaCol <- sym("padj")
        lfcCol <- sym("log2FoldChange")

        ## Coerce to minimal tibble.
        data <- as(object, "tbl_df")
        data <- select(data, !!!syms(c("rowname", "log2FoldChange", "padj")))

        ## Apply alpha cutoff.
        data <- filter(data, !!alphaCol < !!alpha)

        ## Apply LFC threshold cutoff.
        if (lfcThreshold > 0L) {
            data <- filter(
                data,
                !!lfcCol > UQ(lfcThreshold) | !!lfcCol < -UQ(lfcThreshold)
            )
        }

        ## Apply directional filtering.
        if (direction == "up") {
            data <- filter(data, !!lfcCol > 0L)
        } else if (direction == "down") {
            data <- filter(data, !!lfcCol < 0L)
        }

        ## Arrange table by adjusted P value.
        data <- arrange(data, !!alphaCol)

        deg <- pull(data, "rowname")
        status <- paste(
            length(deg),
            switch(
                EXPR = direction,
                up = "upregulated",
                down = "downregulated",
                both = "differentially expressed"
            ),
            "genes detected."
        )

        if (!hasLength(deg)) {
            warning(status)
        } else {
            message(status)
        }

        deg
    }



#' @rdname deg
#' @export
setMethod(
    f = "deg",
    signature = signature("DESeqResults"),
    definition = `deg,DESeqResults`
)



## Updated 2019-07-23.
`deg,DESeqAnalysis` <-  # nolint
    function(
        object,
        results,
        direction = c("both", "up", "down")
    ) {
        results <- results(object = object, results = results)
        direction <- match.arg(direction)
        deg(object = results, direction = direction)
    }



#' @rdname deg
#' @export
setMethod(
    f = "deg",
    signature = signature("DESeqAnalysis"),
    definition = `deg,DESeqAnalysis`
)
