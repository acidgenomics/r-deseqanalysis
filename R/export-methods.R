#' @name export
#' @inherit brio::export
#' @inheritParams brio::export
#' @inheritParams params
#' @examples
#' data(deseq)
#'
#' ## DESeqAnalysis ====
#' export(deseq, dir = "example")
NULL



#' @importFrom bioverbs export
#' @aliases NULL
#' @export
bioverbs::export



.exportDESeqDataSet <- function(x, dir, compress, humanize) {
    # Using the inherited SummarizedExperiment method here.
    assert(is(x, "DESeqAnalysis"))
    message("Exporting DESeqDataSet.")
    export(
        x = as(x, "DESeqDataSet"),
        name = "data",
        dir = dir,
        compress = compress,
        humanize = humanize
    )
}



.exportDESeqTransform <- function(x, dir, compress, humanize) {
    # Using the inherited SummarizedExperiment method here.
    assert(is(x, "DESeqAnalysis"))
    message("\nExporting DESeqTransform.")
    export(
        x = as(x, "DESeqTransform"),
        name = "transform",
        dir = dir,
        compress = compress,
        humanize = humanize
    )
}



# Here we are defining an internal function that works on both
# unshrunken (results) and shrunken (lfcShrink) results. We're using
# inherited global variables here for more compact code.
.exportDESeqResultsList <- function(
    x,
    slotName = c("results", "lfcShrink"),
    dir,
    compress,
    humanize
) {
    assert(is(x, "DESeqAnalysis"))
    slotName <- match.arg(slotName)

    # Get the DESeqDataSet.
    data <- as(x, "DESeqDataSet")
    # Humanize rownames and colnames, if desired.
    if (isTRUE(humanize)) {
        data <- humanize(data)
    }
    dimnames <- dimnames(data)

    # Get the DESeqResults list.
    list <- slot(x, name = slotName)
    assert(is.list(list))
    if (!is(list[[1L]], "DESeqResults")) {
        message(paste(
            slotName, "does not contain DESeqResults.",
            "Skipping export."
        ))
        return(NULL)
    }

    message(paste0("\nExporting ", slotName, "."))
    mapply(
        name = names(list),
        x = list,
        MoreArgs = list(
            dir = file.path(dir, slotName),
            compress = compress,
            humanize = humanize
        ),
        FUN = function(name, x, dir, compress, humanize) {
            file <- file.path(dir, paste0(name, ".csv"))
            if (isTRUE(compress)) {
                file <- paste0(file, ".gz")
            }
            if (isTRUE(humanize)) {
                x[["geneID"]] <- rownames(x)
                rownames(x) <- dimnames[[1L]]
            }
            export(x, file = file)
        },
        SIMPLIFY = TRUE,
        USE.NAMES = TRUE
    )
}



# TODO Improve the messages here.
# Here we are looping across each contrast and writing out DEG tables.
# NOTE: This step picks shrunken LFCs over unshrunken if slotted.
# NOTE: We don't need to support humanize here because `geneName` is required.
.exportResultsTables <- function(x, dir, compress) {
    message("\nExporting results tables.")
    assert(is(x, "DESeqAnalysis"))
    dir <- file.path(dir, "resultsTables")
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
            files <- file.path(
                dir,
                results,
                paste0(names(resTbl), ".csv")
            )
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



export.DESeqAnalysis <-  # nolint
    function(
        x,
        name = NULL,
        dir = ".",
        compress = FALSE,
        humanize = FALSE
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
        files[["data"]] <- do.call(
            what = .exportDESeqDataSet,
            args = list(
                x = x,
                dir = dir,
                compress = compress,
                humanize = humanize
            )
        )

        # DESeqTransform.
        files[["transform"]] <- do.call(
            what = .exportDESeqTransform,
            args = list(
                x = x,
                dir = dir,
                compress = compress,
                humanize = humanize
            )
        )

        # DESeqResults.
        # Here we are writing out both the unshrunken and shrunken values.
        files[["results"]] <- do.call(
            what = .exportDESeqResultsList,
            args = list(
                x = x,
                slotName = "results",
                dir = dir,
                compress = compress,
                humanize = humanize
            )
        )
        files[["lfcShrink"]] <- do.call(
            what = .exportDESeqResultsList,
            args = list(
                x = x,
                slotName = "lfcShrink",
                dir = dir,
                compress = compress,
                humanize = humanize
            )
        )

        # DEG results tables.
        files[["resultsTables"]] <- do.call(
            what = .exportResultsTables,
            args = list(
                x = x,
                dir = dir,
                compress = compress
            )
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
