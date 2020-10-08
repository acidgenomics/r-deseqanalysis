#' @name baseMeanThreshold
#' @inherit AcidGenerics::baseMeanThreshold
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
#' @importFrom AcidGenerics baseMeanThreshold
#' @usage baseMeanThreshold(object, ...)
#' @export
NULL

#' @rdname baseMeanThreshold
#' @name baseMeanThreshold<-
#' @importFrom AcidGenerics baseMeanThreshold<-
#' @usage baseMeanThreshold(object, ...) <- value
#' @export
NULL



## Updated 2020-08-04.
`baseMeanThreshold,DESeqAnalysis` <-  # nolint
    function(object) {
        x <- metadata(object)[["baseMeanThreshold"]]
        if (is.null(x)) {
            x <- 0L
        }
        assert(isNumber(x), isNonNegative(x))
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
        assert(isNumber(value), isNonNegative(value))
        metadata(object)[["baseMeanThreshold"]] <- value
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



## Updated 2020-08-04.
`baseMeanThreshold<-,DESeqAnalysis,NULL` <-  # nolint
    function(object, value) {
        metadata(object)[["baseMeanThreshold"]] <- value
        object
    }



#' @rdname baseMeanThreshold
#' @export
setReplaceMethod(
    f = "baseMeanThreshold",
    signature = signature(
        object = "Annotated",
        value = "NULL"
    ),
    definition = `baseMeanThreshold<-,DESeqAnalysis,NULL`
)



## Updated 2020-08-04.
`baseMeanThreshold,DESeqResults` <-  # nolint
    `baseMeanThreshold,DESeqAnalysis`



#' @rdname baseMeanThreshold
#' @export
setMethod(
    f = "baseMeanThreshold",
    signature = signature("DESeqResults"),
    definition = `baseMeanThreshold,DESeqResults`
)
