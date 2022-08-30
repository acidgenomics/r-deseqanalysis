#' @name export
#' @inherit AcidExperiment::export description params return title
#' @note Updated 2022-05-17.
#'
#' @details
#' Size-factor normalized coutns and FPKM values are calculated on the fly and
#' exported automatically.
#'
#' @param object
#' Object.
#'
#' @param con `character(1)`.
#' Directory path.
#'
#' @param ... Additional arguments.
#'
#' @examples
#' data(deseq)
#'
#' ## DESeqAnalysis ====
#' con <- AcidBase::tempdir2()
#' x <- export(deseq, con = con)
#' print(x)
#' AcidBase::unlink2(con)
NULL



## Updated 2022-05-17.
.exportResultsMatrices <-
    function(object,
             dir,
             compress,
             overwrite,
             quiet) {
        assert(
            is(object, "DESeqAnalysis"),
            isString(dir),
            isFlag(compress),
            isFlag(overwrite),
            isFlag(quiet)
        )
        ## Loop across all possible values to export.
        ## e.g. `"log2FoldChange"`, `"stat"`, `"alpha"`.
        values <- eval(formals(`resultsMatrix,DESeqAnalysis`)[["value"]])
        list <- lapply(
            X = values,
            FUN = function(value) {
                resultsMatrix(object = object, value = value, rowData = TRUE)
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
        out <- Map(
            f = export,
            object = list,
            con = files,
            MoreArgs = list(
                "overwrite" = overwrite,
                "quiet" = quiet
            )
        )
        out <- unlist(out, recursive = FALSE)
        out
    }



## Here we are looping across each contrast and writing out DEG tables. We
## don't need to support humanize mode because `geneName` is required.
##
## Updated 2022-05-17.
.exportResultsTables <-
    function(object,
             dir,
             compress,
             overwrite,
             quiet) {
        assert(
            is(object, "DESeqAnalysis"),
            isFlag(compress),
            isFlag(overwrite),
            isFlag(quiet)
        )
        resultsNames <- resultsNames(object)
        out <- lapply(
            X = resultsNames,
            FUN = function(i) {
                data <- resultsTables(
                    object = object,
                    i = i,
                    extra = TRUE
                )
                if (is.null(data)) {
                    return(invisible(NULL))
                }
                files <- file.path(dir, i, paste0(names(data), ".csv"))
                if (isTRUE(compress)) {
                    files <- paste0(files, ".gz")
                }
                out <- Map(
                    f = export,
                    object = data,
                    con = files,
                    MoreArgs = list(
                        "overwrite" = overwrite,
                        "quiet" = quiet
                    )
                )
                out <- unlist(out, recursive = FALSE)
                out
            }
        )
        names(out) <- resultsNames
        out
    }



## Updated 2021-10-15.
`export,DESeqAnalysis` <- # nolint
    function(object,
             con,
             format, # NULL
             compress = getOption(
                 x = "acid.export.compress",
                 default = FALSE
             ),
             overwrite = getOption(
                 x = "acid.overwrite",
                 default = TRUE
             ),
             quiet = getOption(
                 x = "acid.quiet",
                 default = FALSE
             )) {
        validObject(object)
        if (missing(format)) {
            format <- NULL
        }
        assert(
            isString(con),
            is.null(format),
            isFlag(compress),
            isFlag(overwrite),
            isFlag(quiet)
        )
        dir <- initDir(con)
        files <- list()
        ## DESeqDataSet.
        alert(sprintf(
            "Exporting {.cls %s} to {.path %s}.",
            "DESeqDataSet", "data"
        ))
        files[["data"]] <-
            export(
                object = as(object, "DESeqDataSet"),
                con = file.path(dir, "data"),
                compress = compress,
                overwrite = overwrite,
                quiet = quiet
            )
        ## DESeqTransform.
        alert(sprintf(
            "Exporting {.cls %s} to {.path %s}.",
            "DESeqTransform", "transform"
        ))
        files[["transform"]] <-
            export(
                object = as(object, "DESeqTransform"),
                con = file.path(dir, "transform"),
                compress = compress,
                overwrite = overwrite,
                quiet = quiet
            )
        ## DESeqResults tables.
        alert(sprintf(
            "Exporting {.var %s} tables to {.path %s}.",
            "DESeqResults", "resultsTables"
        ))
        files[["resultsTables"]] <-
            .exportResultsTables(
                object = object,
                dir = file.path(dir, "resultsTables"),
                compress = compress,
                overwrite = overwrite,
                quiet = quiet
            )
        ## Combined DESeqResults matrices.
        alert(sprintf(
            "Exporting {.cls %s} matrices to {.path %s}.",
            "DESeqResults", "resultsMatrices"
        ))
        files[["resultsMatrices"]] <-
            .exportResultsMatrices(
                object = object,
                dir = file.path(dir, "resultsMatrices"),
                compress = compress,
                overwrite = overwrite,
                quiet = quiet
            )
        ## Return file list.
        invisible(files)
    }



## Inheriting the SummarizedExperiment method internally here.
## Only export the raw and normalized counts.
## Skip exporting other assays, including mu, H, cooks.
##
## Updated 2022-05-17.
`export,DESeqDataSet` <- # nolint
    function(object,
             con,
             format, # NULL
             compress = getOption(
                 x = "acid.export.compress",
                 default = FALSE
             ),
             overwrite = getOption(
                 x = "acid.overwrite",
                 default = TRUE
             ),
             quiet = getOption(
                 x = "acid.quiet",
                 default = FALSE
             )) {
        validObject(object)
        if (missing(format)) {
            format <- NULL
        }
        assert(
            isString(con),
            is.null(format),
            isFlag(compress),
            isFlag(overwrite),
            isFlag(quiet)
        )
        ## Generate additional matrices on the fly.
        rse <- as(object, "RangedSummarizedExperiment")
        assays <- SimpleList(
            "counts" = counts(object, normalized = FALSE),
            "normalized" = counts(object, normalized = TRUE),
            "fpkm" = fpkm(object)
        )
        assays(rse) <- assays
        export(
            object = rse,
            con = con,
            compress = compress,
            overwrite = overwrite,
            quiet = quiet
        )
    }



## Updated 2022-05-17.
`export,DESeqAnalysis,deprecated` <- # nolint
    methodFunction(
        f = "export",
        signature = signature(
            object = "SummarizedExperiment",
            con = "missingOrNULL",
            format = "missingOrNULL"
        ),
        package = "AcidExperiment"
    )

## Updated 2022-05-17.
`export,DESeqDataSet,deprecated` <- # nolint
    methodFunction(
        f = "export",
        signature = signature(
            object = "SummarizedExperiment",
            con = "missingOrNULL",
            format = "missingOrNULL"
        ),
        package = "AcidExperiment"
    )



#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature(
        object = "DESeqAnalysis",
        con = "character",
        format = "missingOrNULL"
    ),
    definition = `export,DESeqAnalysis`
)

#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature(
        object = "DESeqAnalysis",
        con = "missingOrNULL",
        format = "missingOrNULL"
    ),
    definition = `export,DESeqAnalysis,deprecated`
)

#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature(
        object = "DESeqDataSet",
        con = "character",
        format = "missingOrNULL"
    ),
    definition = `export,DESeqDataSet`
)

#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature(
        object = "DESeqDataSet",
        con = "missingOrNULL",
        format = "missingOrNULL"
    ),
    definition = `export,DESeqDataSet,deprecated`
)
