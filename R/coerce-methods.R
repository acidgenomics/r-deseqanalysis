#' Coercion methods
#'
#' @name coerce
#' @exportMethod coerce
#' @note Updated 2022-03-30.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @section DESeqAnalysis:
#'
#' Supported coercion methods will extract any of these internal objects:
#'
#' - `DESeqDataSet`.
#' - `DESeqTransform`.
#' - `DESeqResultsList`.
#'
#' @return Modified object, of desired conversion class.
#'
#' @examples
#' data(deseq)
#'
#' ## DESeqAnalysis ====
#' dds <- as.DESeqDataSet(deseq)
#' class(dds)
#' dt <- as.DESeqTransform(deseq)
#' class(dt)
NULL



## Updated 2022-03-30.
`as.DESeqDataSet,DESeqAnalysis` <-
    function(x) {
        as(object = x, Class = "DESeqDataSet")
    }

## Updated 2022-03-30.
`as.DESeqTransform,DESeqAnalysis` <-
    function(x) {
        as(object = x, Class = "DESeqTransform")
    }



## Updated 2022-03-30.
`coerce,DESeqAnalysis,DESeqDataSet` <-
    function(from) {
        validObject(from)
        to <- slot(from, "data")
        validObject(to)
        to
    }

## Updated 2022-03-30.
`coerce,DESeqAnalysis,DESeqTransform` <-
    function(from) {
        validObject(from)
        to <- slot(from, "transform")
        validObject(to)
        to
    }

## Updated 2022-03-30.
`coerce,DESeqAnalysis,DESeqResultsList` <-
    function(from) {
        validObject(from)
        to <- DESeqResultsList(from, quiet = TRUE)
        to
    }



#' @rdname coerce
#' @export
setMethod(
    f = "as.DESeqDataSet",
    signature = signature(x = "DESeqAnalysis"),
    definition = `as.DESeqDataSet,DESeqAnalysis`
)

#' @rdname coerce
#' @export
setMethod(
    f = "as.DESeqTransform",
    signature = signature(x = "DESeqAnalysis"),
    definition = `as.DESeqTransform,DESeqAnalysis`
)



#' @rdname coerce
#' @name coerce,DESeqAnalysis,DESeqDataSet-method
setAs(
    from = "DESeqAnalysis",
    to = "DESeqDataSet",
    def = `coerce,DESeqAnalysis,DESeqDataSet`
)

#' @rdname coerce
#' @name coerce,DESeqAnalysis,DESeqTransform-method
setAs(
    from = "DESeqAnalysis",
    to = "DESeqTransform",
    def = `coerce,DESeqAnalysis,DESeqTransform`
)

#' @rdname coerce
#' @name coerce,DESeqAnalysis,DESeqResultsList-method
setAs(
    from = "DESeqAnalysis",
    to = "DESeqResultsList",
    def = `coerce,DESeqAnalysis,DESeqResultsList`
)
