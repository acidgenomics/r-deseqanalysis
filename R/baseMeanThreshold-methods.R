#' @name baseMeanThreshold
#' @inherit acidgenerics::baseMeanThreshold
#' @note Updated 2020-08-04.
#' @param ... Additional arguments.
#' @examples
#' data(deseq)
#'
#' ## DESeqAnalysis ====
#' object <- deseq
#' baseMeanThreshold(object) <- 100L
#' baseMeanThreshold(object)
NULL



#' @rdname baseMeanThreshold
#' @name baseMeanThreshold
#' @importFrom acidgenerics baseMeanThreshold
#' @usage baseMeanThreshold(object, ...)
#' @export
NULL

#' @rdname baseMeanThreshold
#' @name baseMeanThreshold<-
#' @importFrom acidgenerics baseMeanThreshold<-
#' @usage baseMeanThreshold(object, ...) <- value
#' @export
NULL



`baseMeanThreshold,DESeqAnalysis` <-  # nolint
    function(object) {
        validObject(object)
        x <- metadata(object)[["baseMeanThreshold"]]
        if (is.null(x)) {
            x <- 0L
        }
        assert(
            isNumber(x),
            isNonNegative(x)
        )
        x
    }



#' @rdname baseMeanThreshold
#' @export
setMethod(
    f = "baseMeanThreshold",
    signature = signature("DESeqAnalysis"),
    definition = `baseMeanThreshold,DESeqAnalysis`
)



## Updated 2020-08-04.
`baseMeanThreshold<-,DESeqAnalysis,numeric` <-  # nolint
    function(object, value) {
        assert(isAlpha(value))
        metadata(object)[["baseMeanThreshold"]] <- value
        validObject(object)
        object
    }



#' @rdname baseMeanThreshold
#' @export
setReplaceMethod(
    f = "baseMeanThreshold",
    signature = signature(
        object = "DESeqAnalysis",
        value = "numeric"
    ),
    definition = `baseMeanThreshold<-,DESeqAnalysis,numeric`
)
