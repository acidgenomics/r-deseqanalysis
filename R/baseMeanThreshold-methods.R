## FIXME NEED TO RETHINK THIS WITH DESEQRESULTSLIST.
## FIXME NEED TO RETHINK THIS WITH DESEQANALYSISLIST SUPPORT.



#' @name baseMeanThreshold
#' @inherit AcidGenerics::baseMeanThreshold
#' @note Updated 2021-03-03.
#' @param ... Additional arguments.
#' @examples
#' data(deseq)
#'
#' ## DESeqAnalysis ====
#' object <- deseq
#' baseMeanThreshold(object) <- 100L
#' baseMeanThreshold(object)
NULL



## Updated 2021-03-03.
`baseMeanThreshold,DESeqResults` <-  # nolint
    function(object) {
        x <- metadata(object)[["baseMeanThreshold"]]
        if (is.null(x)) {
            x <- 0L
        }
        assert(isNumber(x), isNonNegative(x))
        x
    }



## Updated 2021-03-03.
`baseMeanThreshold,DESeqResultsList` <-  # nolint
    function(object) {
        x <- metadata(object)[["baseMeanThreshold"]]
        if (is.null(x)) {
            x <- metadata(object[[1L]])[["baseMeanThreshold"]]
        }
        if (is.null(x)) {
            x <- 0L
        }
        assert(isNumber(x), isNonNegative(x))
        x
    }



## FIXME RETHINK THIS ONE...
## Updated 2021-03-03.
`baseMeanThreshold,DESeqAnalysis` <-  # nolint
    `baseMeanThreshold,DESeqResults`



## Updated 2021-03-03.
`baseMeanThreshold<-,DESeqResults,numeric` <-  # nolint
    function(object, value) {
        assert(isNumber(value), isNonNegative(value))
        metadata(object)[["baseMeanThreshold"]] <- value
        object
    }



## Updated 2021-03-03.
`baseMeanThreshold<-,DESeqResults,NULL` <-  # nolint
    function(object, value) {
        metadata(object)[["baseMeanThreshold"]] <- value
        object
    }



## Updated 2021-03-03.
`baseMeanThreshold<-,DESeqAnalysis,numeric` <-  # nolint
    `baseMeanThreshold<-,DESeqResults,numeric`



## Updated 2021-03-03.
`baseMeanThreshold<-,DESeqAnalysis,NULL` <-  # nolint
    `baseMeanThreshold<-,DESeqResults,NULL`



#' @rdname baseMeanThreshold
#' @export
setMethod(
    f = "baseMeanThreshold",
    signature = signature("DESeqResults"),
    definition = `baseMeanThreshold,DESeqResults`
)



#' @rdname baseMeanThreshold
#' @export
setMethod(
    f = "baseMeanThreshold",
    signature = signature("DESeqAnalysis"),
    definition = `baseMeanThreshold,DESeqAnalysis`
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
