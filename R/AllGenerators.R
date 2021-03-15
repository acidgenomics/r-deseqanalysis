#' @include AllGenerics.R
NULL



## DESeqAnalysis ===============================================================
#' @inherit DESeqAnalysis-class title description return
#' @export
#' @note Updated 2021-03-15.
#'
#' @param data `DESeqDataSet`.
#' @param transform `DESeqTransform`.
#'   [DESeq2::varianceStabilizingTransformation()] recommended by default.
#' @param results `list` or single `DESeqResults`.
#'   One or more unshrunken `DESeqResults`.
#'   Assign the [DESeq2::results()] return here.
#' @param lfcShrink `list`, single `DESeqResults`, or `NULL`.
#'   *Optional*. One or more shrunken `DESeqResults`.
#'   Assign the [DESeq2::lfcShrink()] return here.
#'
#' @examples
#' data <- DESeq2::makeExampleDESeqDataSet()
#' rowRanges <- basejump::emptyRanges(names = rownames(data))
#' mcols(rowRanges)[["geneId"]] <- paste0("id", seq_len(length(rowRanges)))
#' mcols(rowRanges)[["geneName"]] <- paste0("name", seq_len(length(rowRanges)))
#' rowRanges(data) <- rowRanges
#' data <- DESeq2::DESeq(data)
#' class(data)
#'
#' transform <- DESeq2::varianceStabilizingTransformation(data)
#' class(transform)
#'
#' resultsNames(data)
#' name <- resultsNames(data)[[2L]]
#' results <- DESeq2::results(data, name = name)
#' class(results)
#'
#' lfcShrink <- DESeq2::lfcShrink(dds = data, res = results, coef = 2L)
#'
#' results <- list(results)
#' names(results) <- name
#'
#' lfcShrink <- list(lfcShrink)
#' names(lfcShrink) <- name
#'
#' object <- DESeqAnalysis(
#'     data = data,
#'     transform = transform,
#'     results = results,
#'     lfcShrink = lfcShrink
#' )
#' print(object)
DESeqAnalysis <-  # nolint
    function(
        data,
        transform,
        results,
        lfcShrink = NULL
    ) {
        ## Allow input of single `DESeqResults`.
        if (is(results, "DESeqResults")) {
            results <- .coerceResultsToList(results)
        }
        if (is(lfcShrink, "DESeqResults")) {
            lfcShrink <- .coerceResultsToList(lfcShrink)
        }
        ## Automatically convert `lfcShrink = NULL` to empty list.
        if (is.null(lfcShrink)) {
            lfcShrink <- list()
        }
        new(
            Class = "DESeqAnalysis",
            data = data,
            transform = transform,
            results = results,
            lfcShrink = lfcShrink,
            metadata = list("packageVersion" = .pkgVersion)
        )
    }



## DESeqAnalysisList ===========================================================
#' @name DESeqAnalysisList
#' @inherit DESeqAnalysisList-class title description return
#' @note Updated 2021-03-15.
#'
#' @param ... `DESeqAnalysis` objects or named `list`.
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



## DESeqResultsList ============================================================
#' @name DESeqResultsList
#' @inherit DESeqResultsList-class title description return
#' @note Updated 2021-03-12.
#'
#' @param ... `DESeqResults` objects or named `list`.
#'
#' @examples
#' data(deseq)
#' x <- DESeqResultsList(deseq)
#' print(names(x))
NULL



## Updated 2021-03-09.
`DESeqResultsList,DESeqAnalysis` <-  # nolint
    function(
        object,
        lfcShrink = NULL,
        quiet = FALSE
    ) {
        validObject(object)
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
            stop(
                "Shrunken LFC values were requested, ",
                "but object does not contain DESeqResults ",
                "defined in 'lfcShrink' slot.\n",
                "Set 'lfcShrink(object) <- NULL'."
            )
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
## Updated 2021-03-10.
`DESeqResultsList,DESeqAnalysisList` <-  # nolint
    function(object, ...) {
        x <- lapply(X = object, FUN = DESeqResultsList, ...)
        x <- lapply(X = x, FUN = as.list)
        x <- unlist(x, recursive = FALSE, use.names = TRUE)
        new(Class = "DESeqResultsList", x)
    }



## Updated 2021-03-12.
`DESeqResultsList,SimpleList` <-  # nolint
    function(object) {
        new(Class = "DESeqResultsList", object)
    }



## Updated 2021-03-12.
`DESeqResultsList,list` <-  # nolint
    `DESeqResultsList,SimpleList`



## Updated 2021-03-08.
`DESeqResultsList,missing` <-  # nolint
    function(object) {
        new(Class = "DESeqResultsList", list())
    }



#' @rdname DESeqResultsList
#' @export
setMethod(
    f = "DESeqResultsList",
    signature = signature("DESeqAnalysis"),
    definition = `DESeqResultsList,DESeqAnalysis`
)



#' @rdname DESeqResultsList
#' @export
setMethod(
    f = "DESeqResultsList",
    signature = signature("DESeqAnalysisList"),
    definition = `DESeqResultsList,DESeqAnalysisList`
)



#' @rdname DESeqResultsList
#' @export
setMethod(
    f = "DESeqResultsList",
    signature = signature("SimpleList"),
    definition = `DESeqResultsList,SimpleList`
)



#' @rdname DESeqResultsList
#' @export
setMethod(
    f = "DESeqResultsList",
    signature = signature("list"),
    definition = `DESeqResultsList,list`
)



#' @rdname DESeqResultsList
#' @export
setMethod(
    f = "DESeqResultsList",
    signature = signature("missing"),
    definition = `DESeqResultsList,missing`
)
