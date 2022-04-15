#' @name baseMeanThreshold
#' @inherit AcidGenerics::baseMeanThreshold
#' @note Updated 2021-03-12.
#' @param ... Additional arguments.
#' @examples
#' data(deseq)
#'
#' ## DESeqAnalysis ====
#' object <- deseq
#' baseMeanThreshold(object) <- 100L
#' baseMeanThreshold(object)
NULL



## Updated 2021-03-10.
`baseMeanThreshold,DESeqAnalysis` <- # nolint
    function(object) {
        x <- metadata(object)[["baseMeanThreshold"]]
        if (is.null(x)) {
            x <- 0L
        }
        assert(isNumber(x), isNonNegative(x))
        x
    }



## Updated 2021-03-10.
`baseMeanThreshold,DESeqAnalysisList` <- # nolint
    function(object) {
        x <- metadata(object)[["baseMeanThreshold"]]
        if (is.null(x)) {
            x <- baseMeanThreshold(object[[1L]])
        }
        assert(isNumber(x), isNonNegative(x))
        x
    }



## Updated 2021-03-03.
`baseMeanThreshold,DESeqResults` <- # nolint
    `baseMeanThreshold,DESeqAnalysis`



## Updated 2021-03-10.
`baseMeanThreshold,DESeqResultsList` <- # nolint
    function(object) {
        x <- metadata(object)[["baseMeanThreshold"]]
        if (is.null(x)) {
            x <- baseMeanThreshold(object[[1L]])
        }
        assert(isNumber(x), isNonNegative(x))
        x
    }



## Updated 2021-03-12.
`baseMeanThreshold<-,DESeqAnalysis,numeric` <- # nolint
    function(object, value) {
        assert(isNumber(value), isNonNegative(value))
        metadata(object)[["baseMeanThreshold"]] <- value
        object
    }



## Updated 2021-03-12.
`baseMeanThreshold<-,DESeqAnalysis,NULL` <- # nolint
    function(object, value) {
        metadata(object)[["baseMeanThreshold"]] <- value
        object
    }



## Updated 2021-03-12.
`baseMeanThreshold<-,DESeqAnalysisList,numeric` <- # nolint
    `baseMeanThreshold<-,DESeqAnalysis,numeric`



## Updated 2021-03-12.
`baseMeanThreshold<-,DESeqAnalysisList,NULL` <- # nolint
    `baseMeanThreshold<-,DESeqAnalysis,NULL`



## Updated 2021-03-12.
`baseMeanThreshold<-,DESeqResults,numeric` <- # nolint
    `baseMeanThreshold<-,DESeqAnalysis,numeric`



## Updated 2021-03-12.
`baseMeanThreshold<-,DESeqResults,NULL` <- # nolint
    `baseMeanThreshold<-,DESeqAnalysis,NULL`



## Updated 2021-03-12.
`baseMeanThreshold<-,DESeqResultsList,numeric` <- # nolint
    `baseMeanThreshold<-,DESeqResults,numeric`



## Updated 2021-03-12.
`baseMeanThreshold<-,DESeqResultsList,NULL` <- # nolint
    `baseMeanThreshold<-,DESeqResults,NULL`



#' @rdname baseMeanThreshold
#' @export
setMethod(
    f = "baseMeanThreshold",
    signature = signature(object = "DESeqAnalysis"),
    definition = `baseMeanThreshold,DESeqAnalysis`
)

#' @rdname baseMeanThreshold
#' @export
setMethod(
    f = "baseMeanThreshold",
    signature = signature(object = "DESeqAnalysisList"),
    definition = `baseMeanThreshold,DESeqAnalysisList`
)

#' @rdname baseMeanThreshold
#' @export
setMethod(
    f = "baseMeanThreshold",
    signature = signature(object = "DESeqResults"),
    definition = `baseMeanThreshold,DESeqResults`
)

#' @rdname baseMeanThreshold
#' @export
setMethod(
    f = "baseMeanThreshold",
    signature = signature(object = "DESeqResultsList"),
    definition = `baseMeanThreshold,DESeqResultsList`
)



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

#' @rdname baseMeanThreshold
#' @export
setReplaceMethod(
    f = "baseMeanThreshold",
    signature = signature(
        object = "DESeqAnalysis",
        value = "NULL"
    ),
    definition = `baseMeanThreshold<-,DESeqAnalysis,NULL`
)

#' @rdname baseMeanThreshold
#' @export
setReplaceMethod(
    f = "baseMeanThreshold",
    signature = signature(
        object = "DESeqAnalysisList",
        value = "numeric"
    ),
    definition = `baseMeanThreshold<-,DESeqAnalysisList,numeric`
)

#' @rdname baseMeanThreshold
#' @export
setReplaceMethod(
    f = "baseMeanThreshold",
    signature = signature(
        object = "DESeqAnalysisList",
        value = "NULL"
    ),
    definition = `baseMeanThreshold<-,DESeqAnalysisList,NULL`
)

#' @rdname baseMeanThreshold
#' @export
setReplaceMethod(
    f = "baseMeanThreshold",
    signature = signature(
        object = "DESeqResults",
        value = "numeric"
    ),
    definition = `baseMeanThreshold<-,DESeqResults,numeric`
)

#' @rdname baseMeanThreshold
#' @export
setReplaceMethod(
    f = "baseMeanThreshold",
    signature = signature(
        object = "DESeqResults",
        value = "NULL"
    ),
    definition = `baseMeanThreshold<-,DESeqResults,NULL`
)

#' @rdname baseMeanThreshold
#' @export
setReplaceMethod(
    f = "baseMeanThreshold",
    signature = signature(
        object = "DESeqResultsList",
        value = "numeric"
    ),
    definition = `baseMeanThreshold<-,DESeqResultsList,numeric`
)

#' @rdname baseMeanThreshold
#' @export
setReplaceMethod(
    f = "baseMeanThreshold",
    signature = signature(
        object = "DESeqResultsList",
        value = "NULL"
    ),
    definition = `baseMeanThreshold<-,DESeqResultsList,NULL`
)
