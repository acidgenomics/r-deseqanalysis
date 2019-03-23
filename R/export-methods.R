# DESeqTransform can inherit from SummarizedExperiment without modification.
# DESeqResults can inherit from DataFrame without modification.



#' @name export
#' @inherit bioverbs::export
#' @inheritParams params
#' @inheritParams brio::export
#' @examples
#' data(deseq)
#'
#' ## DESeqAnalysis ====
#' export(deseq, dir = "example")
#'
#' ## Clean up.
#' unlink("example", recursive = TRUE)
NULL



#' @importFrom bioverbs export
#' @aliases NULL
#' @export
bioverbs::export



# Internal helpers =============================================================
# Here we are looping across each contrast and writing out DEG tables.
# Note: This step picks shrunken LFCs over unshrunken if slotted.
# Note: We don't need to support humanize here because `geneName` is required.
.exportResultsTables <- function(x, dir = ".", compress = FALSE) {
    assert(is(x, "DESeqAnalysis"))
    resultsNames <- names(x@results)
    out <- lapply(
        X = resultsNames,
        FUN = function(results) {
            resTbl <- resultsTables(
                object = x,
                results = results,
                rowData = TRUE,
                counts = TRUE,
                return = "tbl_df"
            )
            files <- file.path(dir, results, paste0(names(resTbl), ".csv"))
            if (isTRUE(compress)) {
                files <- paste0(files, ".gz")
            }
            mapply(
                x = resTbl,
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



# Exported methods =============================================================
# Inheriting the SummarizedExperiment method internally here.
# Only export the raw and normalized counts.
# Skip exporting other assays, including mu, H, cooks.
export.DESeqDataSet <-  # nolint
    function(x, name = NULL, dir = ".", compress = FALSE) {
        validObject(x)

        call <- standardizeCall()
        assert(isString(name, nullOK = TRUE))
        if (is.null(name)) {
            name <- as.character(call[["x"]])
        }

        normalized <- counts(x, normalized = TRUE)

        rse <- as(x, "RangedSummarizedExperiment")
        assays(rse)[["normalized"]] <- normalized
        assays(rse) <- assays(rse)[c("counts", "normalized")]

        export(x = rse, name = name, dir = dir, compress = compress)
    }



#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature("DESeqDataSet"),
    definition = export.DESeqDataSet
)



export.DESeqAnalysis <-  # nolint
    function(
        x,
        name = NULL,
        dir = ".",
        compress = FALSE
    ) {
        validObject(x)

        call <- standardizeCall()
        assert(isString(name, nullOK = TRUE))
        if (is.null(name)) {
            name <- as.character(call[["x"]])
        }

        # Note that we're combining the dir with name, so we can set
        # subdirectories for each slotted data type (e.g. DESeqDataSet).
        dir <- initDir(file.path(dir, name))
        rm(name)

        files <- list()

        # DESeqDataSet.
        message("Exporting DESeqDataSet.")
        files[["data"]] <- export(
            x = as(x, "DESeqDataSet"),
            name = "data",
            dir = dir,
            compress = compress
        )

        # DESeqTransform.
        message("Exporting DESeqTransform.")
        files[["transform"]] <- export(
            x = as(x, "DESeqTransform"),
            name = "transform",
            dir = dir,
            compress = compress
        )

        # DEG results tables.
        message("Exporting DESeqResults tables.")
        files[["resultsTables"]] <- .exportResultsTables(
            x = x,
            dir = file.path(dir, "resultsTables"),
            compress = compress
        )

        invisible(files)
    }



#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature("DESeqAnalysis"),
    definition = export.DESeqAnalysis
)
