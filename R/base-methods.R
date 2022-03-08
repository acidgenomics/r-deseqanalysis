#' Base methods
#'
#' @name base
#' @keywords internal
#' @note Updated 2020-10-28.
#'
#' @inheritParams AcidRoxygen::params
#' @inheritParams base::colnames
#'
#' @return Varies, dependending on function.
#'
#' @examples
#' data(deseq)
#'
#' head(colnames(deseq))
#' head(rownames(deseq))
#'
#' head(names(deseq))
#'
#' lapply(X = dimnames(deseq), FUN = head)
NULL



## Updated 2020-10-28.
`colnames,DESeqAnalysis` <-  # nolint
    function(x) {
        colnames(as(x, "DESeqDataSet"))
    }

formals(`colnames,DESeqAnalysis`) <- formals(colnames)

#' @rdname base
#' @export
setMethod(
    f = "colnames",
    signature = signature(x = "DESeqAnalysis"),
    definition = `colnames,DESeqAnalysis`
)



## Updated 2020-10-28.
`dim,DESeqAnalysis` <-  # nolint
    function(x) {
        dim(as(x, "DESeqDataSet"))
    }

#' @rdname base
#' @export
setMethod(
    f = "dim",
    signature = signature(x = "DESeqAnalysis"),
    definition = `dim,DESeqAnalysis`
)



## Updated 2020-10-28.
`dimnames,DESeqAnalysis` <-  # nolint
    function(x) {
        dimnames(as(x, "DESeqDataSet"))
    }

#' @rdname base
#' @export
setMethod(
    f = "dimnames",
    signature = signature(x = "DESeqAnalysis"),
    definition = `dimnames,DESeqAnalysis`
)



## Updated 2020-10-28.
`names,DESeqAnalysis` <-  # nolint
    function(x) {
        names(as(x, "DESeqDataSet"))
    }

#' @rdname base
#' @export
setMethod(
    f = "names",
    signature = signature(x = "DESeqAnalysis"),
    definition = `names,DESeqAnalysis`
)



## Updated 2020-10-28.
`rownames,DESeqAnalysis` <-  # nolint
    function(x) {
        rownames(as(x, "DESeqDataSet"))
    }

formals(`rownames,DESeqAnalysis`) <- formals(rownames)

#' @rdname base
#' @export
setMethod(
    f = "rownames",
    signature = signature(x = "DESeqAnalysis"),
    definition = `rownames,DESeqAnalysis`
)
