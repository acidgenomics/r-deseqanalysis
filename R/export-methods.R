## DESeqTransform can inherit from SummarizedExperiment without modification.
## DESeqResults can inherit from DataFrame without modification.
## Need to add method for DESeqAnalysisList.



#' @name export
#' @inherit bioverbs::export
#' @note Size-factor normalized coutns and FPKM values are calculated on the fly
#' and exported automatically.
#' @note Updated 2019-09-11.
#'
#' @inheritParams brio::export
#' @inheritParams params
#' @param ... Additional arguments.
#'
#' @examples
#' data(deseq)
#'
#' ## DESeqAnalysis ====
#' export(deseq, dir = "example")
#'
#' ## Clean up.
#' unlink("example", recursive = TRUE)
NULL



#' @rdname export
#' @name export
#' @importFrom bioverbs export
#' @usage export(object, ...)
#' @export
NULL



## Internal helpers ============================================================
## Here we are looping across each contrast and writing out DEG tables.
## Note: We don't need to support humanize mode because `geneName` is required.
## Updated 2019-07-23.
.exportResultsTables <- function(object, dir, compress, lfcShrink) {
    assert(
        is(object, "DESeqAnalysis"),
        isFlag(compress),
        isFlag(lfcShrink)
    )
    resultsNames <- resultsNames(object)
    out <- lapply(
        X = resultsNames,
        FUN = function(results) {
            resTbl <- resultsTables(
                object = object,
                results = results,
                lfcShrink = lfcShrink,
                extra = TRUE,
                return = "tbl_df"
            )
            files <- file.path(dir, results, paste0(names(resTbl), ".csv"))
            if (isTRUE(compress)) {
                files <- paste0(files, ".gz")
            }
            mapply(
                object = resTbl,
                file = files,
                FUN = export,
                SIMPLIFY = TRUE,
                USE.NAMES = TRUE
            )
        }
    )
    names(out) <- resultsNames
    out
}



## Exported methods ============================================================
## Inheriting the SummarizedExperiment method internally here.
## Only export the raw and normalized counts.
## Skip exporting other assays, including mu, H, cooks.
## Updated 2019-09-11.
`export,DESeqDataSet` <-  # nolint
    function(
        object,
        name = NULL,
        dir = ".",
        compress = FALSE
    ) {
        validObject(object)
        call <- standardizeCall()
        assert(isString(name, nullOK = TRUE))
        if (is.null(name)) {
            name <- as.character(call[["object"]])
        }
        ## Generate additional matrices on the fly.
        rse <- as(object, "RangedSummarizedExperiment")
        assays <- SimpleList(
            counts = counts(object, normalized = FALSE),
            normalized = counts(object, normalized = TRUE),
            fpkm = fpkm(object)
        )
        assays(rse) <- assays
        export(object = rse, name = name, dir = dir, compress = compress)
    }



#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature("DESeqDataSet"),
    definition = `export,DESeqDataSet`
)



## Updated 2019-07-23.
`export,DESeqAnalysis` <-  # nolint
    function(
        object,
        name = NULL,
        dir = ".",
        compress = FALSE,
        lfcShrink = TRUE
    ) {
        validObject(object)
        assert(
            isString(name, nullOK = TRUE),
            isFlag(compress),
            isFlag(lfcShrink)
        )

        call <- standardizeCall()
        assert(isString(name, nullOK = TRUE))
        if (is.null(name)) {
            name <- as.character(call[["object"]])
        }

        ## Note that we're combining the dir with name, so we can set
        ## subdirectories for each slotted data type (e.g. DESeqDataSet).
        dir <- initDir(file.path(dir, name))
        rm(name)

        files <- list()

        ## DESeqDataSet.
        message("Exporting DESeqDataSet.")
        files[["data"]] <-
            export(
                object = as(object, "DESeqDataSet"),
                name = "data",
                dir = dir,
                compress = compress
            )

        ## DESeqTransform.
        message("Exporting DESeqTransform.")
        files[["transform"]] <-
            export(
                object = as(object, "DESeqTransform"),
                name = "transform",
                dir = dir,
                compress = compress
            )

        ## DEG results tables.
        message("Exporting DESeqResults tables.")
        files[["resultsTables"]] <-
            .exportResultsTables(
                object = object,
                dir = file.path(dir, "resultsTables"),
                compress = compress,
                lfcShrink = lfcShrink
            )

        invisible(files)
    }



#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature("DESeqAnalysis"),
    definition = `export,DESeqAnalysis`
)
