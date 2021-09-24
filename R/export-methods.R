## FIXME Need to rework using BiocIO approach.



#' @name export
#' @inherit AcidExperiment::export
#' @note Updated 2021-09-24.
#'
#' @details
#' Size-factor normalized coutns and FPKM values are calculated on the fly and
#' exported automatically.
#'
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



## Updated 2021-08-09.
.exportResultsMatrices <- function(object, dir, compress) {
    assert(
        is(object, "DESeqAnalysis"),
        isFlag(compress)
    )
    values <- eval(formals(`resultsMatrix,DESeqAnalysis`)[["value"]])
    list <- lapply(
        X = values,
        FUN = function(value) {
            resultsMatrix(object, value = value, rowData = TRUE)
        }
    )
    ## Ensure we dynamically remap "alpha" back to "padj" or "svalue".
    values <- vapply(
        X = list,
        FUN = function(x) {
            metadata(x)[["DESeqAnalysis"]][["value"]]
        },
        FUN.VALUE = character(1L),
        USE.NAMES = FALSE
    )
    names(list) <- values
    files <- file.path(dir, paste0(values, ".csv"))
    if (isTRUE(compress)) {
        files <- paste0(files, ".gz")
    }
    ## FIXME Need to rework call here, supporting "con" and "format".
    mapply(
        object = list,
        file = files,
        FUN = export,
        SIMPLIFY = TRUE,
        USE.NAMES = TRUE
    )
}



## Here we are looping across each contrast and writing out DEG tables.
## Note: We don't need to support humanize mode because `geneName` is required.
## Updated 2020-08-04.
.exportResultsTables <- function(object, dir, compress) {
    assert(
        is(object, "DESeqAnalysis"),
        isFlag(compress)
    )
    resultsNames <- resultsNames(object)
    out <- lapply(
        X = resultsNames,
        FUN = function(i) {
            data <- resultsTables(
                object = object,
                i = i,
                extra = TRUE,
                return = "tbl_df"
            )
            if (is.null(data)) {
                return(invisible(NULL))
            }
            files <- file.path(dir, i, paste0(names(data), ".csv"))
            if (isTRUE(compress)) {
                files <- paste0(files, ".gz")
            }
            ## FIXME Need to rework call here, supporting "con" and "format".
            mapply(
                object = data,
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



## Updated 2021-09-24.
`export,DESeqAnalysis` <-  # nolint
    function(
        object,
        con,  # FIXME
        format,  # FIXME
        name = NULL,
        dir = ".",
        compress = FALSE
    ) {
        validObject(object)
        assert(
            isString(name, nullOK = TRUE),
            isFlag(compress)
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
        alert("Exporting {.cls DESeqDataSet} to {.path data}.")
        files[["data"]] <-
            ## FIXME Need to rework call here, supporting "con" and "format".
            export(
                object = as(object, "DESeqDataSet"),
                name = "data",
                dir = dir,
                compress = compress
            )
        ## DESeqTransform.
        alert("Exporting {.var DESeqTransform} to {.path transform}.")
        files[["transform"]] <-
            ## FIXME Need to rework call here, supporting "con" and "format".
            export(
                object = as(object, "DESeqTransform"),
                name = "transform",
                dir = dir,
                compress = compress
            )
        ## DEG results tables.
        alert(
            "Exporting {.var DESeqResults} tables to {.path resultsTables}."
        )
        files[["resultsTables"]] <-
            .exportResultsTables(
                object = object,
                dir = file.path(dir, "resultsTables"),
                compress = compress
            )
        ## Combined results matrices.
        alert(
            "Exporting {.var DESeqResults} matrices to {.path resultsMatrices}."
        )
        files[["resultsMatrices"]] <-
            .exportResultsMatrices(
                object = object,
                dir = file.path(dir, "resultsMatrices"),
                compress = compress
            )
        ## Return file list.
        invisible(files)
    }



## Inheriting the SummarizedExperiment method internally here.
## Only export the raw and normalized counts.
## Skip exporting other assays, including mu, H, cooks.
## Updated 2019-09-11.
`export,DESeqDataSet` <-  # nolint
    function(
        object,
        con,  # FIXME
        format,  # FIXME
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
        ## FIXME Need to rework call here, supporting "con" and "format".
        export(object = rse, name = name, dir = dir, compress = compress)
    }



#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature(
        object = "DESeqAnalysis",
        con = "ANY",  # FIXME
        format = "ANY"  # FIXME
    ),
    definition = `export,DESeqAnalysis`
)

#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature(
        object = "DESeqDataSet",
        con = "ANY",  # FIXME
        format = "ANY"  # FIXME
    ),
    definition = `export,DESeqDataSet`
)
