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



`baseMeanThreshold,Annotated` <-  # nolint
    function(object) {
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
    signature = signature("Annotated"),
    definition = `baseMeanThreshold,Annotated`
)



## Updated 2020-08-04.
`baseMeanThreshold<-,Annotated,numeric` <-  # nolint
    function(object, value) {
        assert(isAlpha(value))
        metadata(object)[["baseMeanThreshold"]] <- value
        object
    }



#' @rdname baseMeanThreshold
#' @export
setReplaceMethod(
    f = "baseMeanThreshold",
    signature = signature(
        object = "Annotated",
        value = "numeric"
    ),
    definition = `baseMeanThreshold<-,Annotated,numeric`
)
