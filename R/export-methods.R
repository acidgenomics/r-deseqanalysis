#' @name export
#' @inherit basejump::export
#' @inheritParams basejump::params
#' @inheritParams params
#' @examples
#' data(deseq)
#'
#' ## DESeqAnalysis ====
#' export(deseq, dir = tempdir())
#'
#' ## DESeqResultsTables ====
#' x <- DESeqResultsTables(deseq)
#' export(x, dir = tempdir())
NULL



#' @importFrom basejump export
#' @aliases NULL
#' @export
basejump::export



# Consider using `methodFormals()` here to get the formals from
# SummarizedExperiment method exported in basejump.
export.DESeqAnalysis <- function(
    x,
    name = NULL,
    dir = ".",
    compress = FALSE,
    human = FALSE
) {
    validObject(x)
    call <- standardizeCall()
    assertIsStringOrNULL(name)
    if (is.null(name)) {
        name <- as.character(call[["x"]])
    }
    # Note that we're combining the dir with name, so we can set subdirectories
    # for each slotted data type (e.g. DESeqDataSet).
    dir <- initDir(file.path(dir, name))
    rm(name)

    files <- list()

    # Using inherited SummarizedExperiment method here.
    message("Exporting DESeqDataSet.")
    data <- slot(x, name = "data")
    stopifnot(is(data, "DESeqDataSet"))
    files[["data"]] <- export(
        x = data,
        name = "data",
        dir = dir,
        compress = compress,
        human = human
    )

    # Only export the assay from this object, as we already have the
    # corresponding `rowRanges()` and `colData()` in the DESeqDataSet.
    message("Exporting DESeqTransform.")
    transform <- slot(x, name = "transform")
    stopifnot(is(transform, "DESeqTransform"))
    files[["transform"]] <- export(
        x = transform,
        name = "transform",
        dir = dir,
        compress = compress,
        human = human
    )

    # Humanize rownames and colnames if necessary.
    if (isTRUE(human)) {
        data <- humanize(data)
    }
    dimnames <- dimnames(data)

    # Define an internal function that work on both unshrunken and shrunken
    # results.
    exportResults <- function(slotName = c("results", "lfcShrink")) {
        slotName <- match.arg(slotName)
        list <- slot(x, name = slotName)
        assert_is_list(list)
        if (!is(list[[1L]], "DESeqResults")) {
            message(paste(
                slotName, "does not contain DESeqResults.",
                "Skipping export."
            ))
            return(NULL)
        }
        message(paste0("Exporting ", slotName, "."))
        if (!has_names(list)) {
            names(list) <- makeNames(vapply(
                X = list,
                FUN = contrastName,
                FUN.VALUE = character(1L)
            ))
        }
        mapply(
            name = names(list),
            x = list,
            MoreArgs = list(
                dir = file.path(dir, slotName),
                compress = compress,
                human = human
            ),
            FUN = function(name, x, dir, compress, human) {
                file <- file.path(dir, paste0(name, ".csv"))
                if (isTRUE(compress)) {
                    file <- paste0(file, ".gz")
                }
                if (isTRUE(human)) {
                    x[["geneID"]] <- rownames(x)
                    rownames(x) <- dimnames[[1L]]
                }
                export(x, file = file)
            },
            SIMPLIFY = TRUE,
            USE.NAMES = TRUE
        )
    }

    files[["results"]] <- exportResults("results")
    files[["lfcShrink"]] <- exportResults("lfcShrink")

    invisible(files)
}



#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature("DESeqAnalysis"),
    definition = export.DESeqAnalysis
)



export.DESeqResultsTables <-  # nolint
    function(x, dir = ".", compress = FALSE) {
        validObject(x)
        dir <- initDir(dir)
        assert_is_a_bool(compress)

        # Prepare the subset tables --------------------------------------------
        deg <- slot(x, "deg")
        assert_is_list(deg)
        assert_are_identical(names(deg), c("up", "down"))

        # Up-regulated, down-regulated, and bidirectional DEGs.
        up <- deg[["up"]]
        down <- deg[["down"]]
        both <- c(up, down)

        # Coerce DESeqResults to DataFrame.
        data <- slot(x, name = "results") %>%
            as("DataFrame")
        rowData <- slot(x, name = "rowRanges") %>%
            # Coerce to data.frame first, to collapse "X" ranges column.
            as.data.frame() %>%
            as("DataFrame")
        counts <- slot(x, name = "counts")
        sampleNames <- slot(x, name = "sampleNames")

        # Row annotations.
        if (!is.null(rowData) && ncol(rowData) > 0L) {
            message("Joining annotations.")
            assert_is_all_of(rowData, "DataFrame")
            rowData <- sanitizeRowData(rowData)
            assert_are_identical(rownames(data), rownames(rowData))
            data <- cbind(data, rowData)
        }

        # Variance-stabilized counts (DESeqTransform).
        if (!is.null(counts) && ncol(counts) > 0L) {
            message("Joining counts.")
            assert_is_matrix(counts)
            assert_are_identical(rownames(data), rownames(counts))
            # Convert to human friendly sample names, if possible.
            if (is.character(sampleNames) && has_length(sampleNames)) {
                message("Mapping human-friendly sample names to counts.")
                assert_has_names(sampleNames)
                assert_are_identical(names(sampleNames), colnames(counts))
                colnames(counts) <- as.character(sampleNames)
            }
            assert_are_disjoint_sets(colnames(data), colnames(counts))
            data <- cbind(data, counts)
        }

        tables <- list(
            all = data,
            deg_up = data[up, , drop = FALSE],
            deg_down = data[down, , drop = FALSE],
            deg_both = data[both, , drop = FALSE]
        )

        # Local files (required) -----------------------------------------------
        stem <- snake(contrastName(x))
        format <- "csv"
        if (isTRUE(compress)) {
            format <- paste0(format, ".gz")
        }
        ext <- paste0(".", format)
        files <- file.path(dir, paste0(stem, "_", snake(names(tables)), ext))
        names(files) <- names(tables)

        # Write the results tables to local directory.
        message(paste0("Writing ", toString(basename(files)), " to ", dir, "."))
        invisible(mapply(
            x = tables,
            file = files,
            FUN = function(x, file) {
                export(x = x, file = file)
            },
            SIMPLIFY = FALSE,
            USE.NAMES = FALSE
        ))
    }



#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature("DESeqResultsTables"),
    definition = export.DESeqResultsTables
)
