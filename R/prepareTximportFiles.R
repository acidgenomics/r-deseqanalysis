#' Prepare quant files for tximport
#'
#' @export
#' @note Updated 2019-10-09.
#'
#' @param exists `logical(1)`.
#'   Check if requested input exists on disk.
#'
#' @details
#' Runs the following internal comments:
#'
#' 1. Extract sample directory name from quant file using
#'    [`dirname()`][base::dirname] and [`basename()`][base::basename].
#' 2. Autopad zeros, if necessary, via
#'    [`autopadZeros()`][basejump::autopadZeros].
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
#' files <- prepareTximportFiles(files, exists = FALSE)
#' print(files)
prepareTximportFiles <- function(files, exists = TRUE) {
    if (isTRUE(exists)) {
        files <- realpath(files)
    }
    names <- basename(dirname(files))
    names <- autopadZeros(names)
    names <- snakeCase(names)
    names(files) <- names
    files <- files[sort(names)]
    files
}
