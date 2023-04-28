#' Prepare quant files for tximport
#'
#' @export
#' @note Updated 2023-04-28.
#'
#' @param dir `character(1)`.
#' Directory path containing quant files.
#' See [`tximport()`][tximport::tximport] for details.
#'
#' @param type `character(1)`.
#' Type of quant/abundance file to find.
#' Expecting `"quant.sf"` for salmon, `"abundance.h5"` for kallisto.
#'
#' @param makeNames `character(1)`.
#' Syntactic name function to apply on sample names.
#' Uses [`match.arg()`][base::match.arg] internally.
#'
#' @details
#' Runs the following internal comments:
#'
#' 1. Extract sample directory name from quant file using
#' [`dirname()`][base::dirname] and [`basename()`][base::basename].
#' 2. Autopad zeros, if necessary, via
#' [`autopadZeros()`][basejump::autopadZeros].
#' 3. Sanitizes names with [`snakeCase()`][syntactic::snakeCase].
#' 4. Sorts files alphabetically.
#'
#' @return `character`.
#' Return quant file paths, with valid sample names automatically applied.
#'
#' @seealso
#' - tximport vignette
#'
#' @examples
#' files <- c(
#'     file.path("salmon", "1-sample-A", "quant.sf"),
#'     file.path("salmon", "2-sample-B", "quant.sf")
#' )
#' print(files)
#' files <- prepareTximportFiles(dir = "salmon", type = "salmon")
#' print(files)
prepareTximportFiles <-
    function(dir,
             type = c("salmon", "kallisto"),
             makeNames = c("makeNames", "snakeCase", "camelCase")) {
        makeNames <- get(
            x = match.arg(makeNames),
            envir = asNamespace("basejump"),
            inherits = TRUE
        )
        assert(
            isADir(dir),
            is.function(makeNames)
        )
        type <- match.arg(type)
        pattern <- switch(
            EXPR = type,
            "salmon" = "quant.sf",
            "kallisto" = "abundance.h5"
        )
        files <- sort(list.files(
            path = dir,
            pattern = pattern,
            full.names = TRUE,
            recursive = TRUE,
            ignore.case = TRUE
        ))
        assert(
            hasLength(files),
            msg = sprintf(
                "Failed to detect {.file %s} in {.dir %s}.",
                pattern, dir
            )
        )
        files <- realpath(files)
        names <- basename(dirname(files))
        names <- autopadZeros(names)
        names <- makeNames(names)
        names(files) <- names
        files <- files[sort(names)]
        files
    }
