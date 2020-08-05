#' @name interestingGroups
#' @inherit acidgenerics::interestingGroups
#' @note Updated 2020-08-04.
#'
#' @inheritParams acidroxygen::params
#' @param ... Additional arguments.
#'
#' @examples
#' data(deseq)
#'
#' ## DESeqAnalysis ====
#' intgroup <- interestingGroups(deseq)
#' print(intgroup)
#' ## Assignment support.
#' interestingGroups(deseq) <- intgroup
#' interestingGroups(deseq)
NULL



#' @rdname interestingGroups
#' @name interestingGroups
#' @importFrom basejump interestingGroups
#' @usage interestingGroups(object, ...)
#' @export
NULL

#' @rdname interestingGroups
#' @name interestingGroups<-
#' @importFrom basejump interestingGroups<-
#' @usage interestingGroups(object, ...)  <- value
#' @export
NULL



## Updated 2020-08-04.
`interestingGroups,DESeqAnalysis` <-  # nolint
    function(object) {
        dds <- as(object, "DESeqDataSet")
        interestingGroups(dds)
    }



#' @rdname interestingGroups
#' @export
setMethod(
    f = "interestingGroups",
    signature = signature("DESeqAnalysis"),
    definition = `interestingGroups,DESeqAnalysis`
)



## Updated 2020-08-04.
`interestingGroups<-,DESeqAnalysis,character` <-  # nolint
    function(object, value) {
        data <- slot(object, "data")
        transform <- slot(object, "transform")
        interestingGroups(data) <- value
        interestingGroups(transform) <- value
        slot(object, "data") <- data
        slot(object, "transform") <- transform
        object
    }



#' @rdname interestingGroups
#' @export
setReplaceMethod(
    f = "interestingGroups",
    signature = signature(
        object = "DESeqAnalysis",
        value = "character"
    ),
    definition = `interestingGroups<-,DESeqAnalysis,character`
)



`interestingGroups<-,DESeqAnalysis,NULL` <-  # nolint
    `interestingGroups<-,DESeqAnalysis,character`



#' @rdname interestingGroups
#' @export
setReplaceMethod(
    f = "interestingGroups",
    signature = signature(
        object = "DESeqAnalysis",
        value = "NULL"
    ),
    definition = `interestingGroups<-,DESeqAnalysis,NULL`
)
