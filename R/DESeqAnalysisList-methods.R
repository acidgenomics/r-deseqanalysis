#' @name DESeqAnalysisList
#' @inherit DESeqAnalysisList-class title description return
#' @note Updated 2021-03-15.
#'
#' @inheritParams AcidRoxygen::params
#'
#' @examples
#' data(deseq)
#' x <- DESeqAnalysisList(list("object1" = deseq, "object2" = deseq))
#' x
NULL



## Updated 2021-03-12.
`DESeqAnalysisList,SimpleList` <-  # nolint
    function(object) {
        assert(hasLength(object), hasNames(object))
        new(Class = "DESeqAnalysisList", object)
    }



## Updated 2021-03-12.
`DESeqAnalysisList,list` <-  # nolint
    `DESeqAnalysisList,SimpleList`



## Updated 2021-03-08.
`DESeqAnalysisList,missing` <-  # nolint
    function(object) {
        new(Class = "DESeqAnalysisList", list())
    }



#' @rdname DESeqAnalysisList
#' @export
setMethod(
    f = "DESeqAnalysisList",
    signature = signature("SimpleList"),
    definition = `DESeqAnalysisList,SimpleList`
)

#' @rdname DESeqAnalysisList
#' @export
setMethod(
    f = "DESeqAnalysisList",
    signature = signature("list"),
    definition = `DESeqAnalysisList,list`
)

#' @rdname DESeqAnalysisList
#' @export
setMethod(
    f = "DESeqAnalysisList",
    signature = signature("missing"),
    definition = `DESeqAnalysisList,missing`
)
