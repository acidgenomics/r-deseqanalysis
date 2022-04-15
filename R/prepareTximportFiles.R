#' Prepare quant files for tximport
#'
#' @export
#' @note Updated 2019-10-09.
#'
#' @param files `character`.
#' Quant file paths (e.g. "quant.sf" for salmon, "abundance.h5" for kallisto).
#' See [`tximport()`][tximport::tximport] for details.
#' @param exists `logical(1)`.
#' Check if requested input exists on disk.
#' Runs [`realpath()`][basejump::realpath] internally.
#' @param makeNames `character(1)`.
#' Syntactic name function to apply on sample names.
#' Uses [`match.arg()`][base::match.arg] internally.
#' See basejump toolkit for details.
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
#' @examples
#' files <- c(
#'     file.path("salmon", "1-sample-A", "quant.sf"),
#'     file.path("salmon", "2-sample-B", "quant.sf")
#' )
#' print(files)
#' files <- prepareTximportFiles(files, makeNames = "snakeCase", exists = FALSE)
#' print(files)
prepareTximportFiles <- function(files,
                                 makeNames = c("makeNames", "snakeCase", "camelCase"),
                                 exists = TRUE) {
    makeNames <- get(
        x = match.arg(makeNames),
        envir = asNamespace("basejump"),
        inherits = TRUE
    )
    assert(
        isCharacter(files),
        isFlag(exists),
        is.function(makeNames)
    )
    if (isTRUE(exists)) {
        files <- realpath(files) # nocov
    }
    names <- basename(dirname(files))
    names <- autopadZeros(names)
    names <- makeNames(names)
    names(files) <- names
    files <- files[sort(names)]
    files
}
