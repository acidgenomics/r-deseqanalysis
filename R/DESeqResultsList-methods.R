#' @name DESeqResultsList
#' @inherit DESeqResultsList-class title description return
#' @note Updated 2021-03-15.
#'
#' @inheritParams AcidRoxygen::params
#' @inheritParams params
#'
#' @examples
#' data(deseq)
#' x <- DESeqResultsList(deseq)
#' print(names(x))
NULL



## Updated 2021-03-09.
`DESeqResultsList,DESeqAnalysis` <- # nolint
    function(object,
             lfcShrink = NULL,
             quiet = FALSE) {
        assert(validObject(object))
        if (is.null(lfcShrink)) {
            lfcShrink <- lfcShrink(object)
        }
        assert(
            isFlag(lfcShrink),
            isFlag(quiet)
        )
        if (!isTRUE(lfcShrink)) {
            slotName <- "results"
        } else if (
            isTRUE(lfcShrink) &&
                hasLength(slot(object, name = "lfcShrink"))
        ) {
            slotName <- "lfcShrink"
        } else if (
            isTRUE(lfcShrink) &&
                !hasLength(slot(object, name = "lfcShrink"))
        ) {
            abort(sprintf(
                fmt = paste0(
                    "Shrunken LFC values were requested, ",
                    "but object does not contain {.cls %s} object ",
                    "defined in {.var %s} slot.\n",
                    "Set {.code %s}."
                ),
                "DESeqResults", "lfcShrink",
                "lfcShrink(object) <- NULL"
            ))
        }
        out <- slot(object, name = slotName)
        assert(
            is.list(out),
            hasNames(out)
        )
        if (isFALSE(quiet)) {
            alertInfo(sprintf(
                "Returning %s results from {.var %s} slot.",
                switch(
                    EXPR = slotName,
                    "lfcShrink" = "shrunken",
                    "results" = "unshrunken"
                ),
                slotName
            ))
        }
        out <- SimpleList(out)
        metadata(out) <- list("lfcShrink" = lfcShrink)
        new(Class = "DESeqResultsList", out)
    }



## Coercion to `SimpleList` here doesn't unlist like we'd want here.
## Updated 2021-03-15.
`DESeqResultsList,DESeqAnalysisList` <- # nolint
    function(object, ...) {
        x <- lapply(X = object, FUN = DESeqResultsList, ...)
        x <- lapply(X = x, FUN = as.list)
        x <- unlist(x, recursive = FALSE, use.names = TRUE)
        names(x) <- makeNames(names(x))
        new(Class = "DESeqResultsList", x)
    }



## Updated 2021-03-12.
`DESeqResultsList,SimpleList` <- # nolint
    function(object) {
        new(Class = "DESeqResultsList", object)
    }



## Updated 2021-03-12.
`DESeqResultsList,list` <- # nolint
    `DESeqResultsList,SimpleList`



## Updated 2021-03-08.
`DESeqResultsList,missing` <- # nolint
    function(object) {
        new(Class = "DESeqResultsList", list())
    }



#' @rdname DESeqResultsList
#' @export
setMethod(
    f = "DESeqResultsList",
    signature = signature(object = "DESeqAnalysis"),
    definition = `DESeqResultsList,DESeqAnalysis`
)

#' @rdname DESeqResultsList
#' @export
setMethod(
    f = "DESeqResultsList",
    signature = signature(object = "DESeqAnalysisList"),
    definition = `DESeqResultsList,DESeqAnalysisList`
)

#' @rdname DESeqResultsList
#' @export
setMethod(
    f = "DESeqResultsList",
    signature = signature(object = "SimpleList"),
    definition = `DESeqResultsList,SimpleList`
)

#' @rdname DESeqResultsList
#' @export
setMethod(
    f = "DESeqResultsList",
    signature = signature(object = "list"),
    definition = `DESeqResultsList,list`
)

#' @rdname DESeqResultsList
#' @export
setMethod(
    f = "DESeqResultsList",
    signature = signature(object = "missing"),
    definition = `DESeqResultsList,missing`
)
