#' Match user metadata to file names used for tximport
#'
#' @export
#' @note Updated 2019-10-09.
#'
#' @param files `character`.
#'   Quant file paths passed to [`tximport()`][tximport::tximport].
#'   Sanitize return from [prepareTximportFiles()] is recommended.
#' @param metadata `data.frame`.
#'   User-defined metadata. The function assumes that sample identifiers are
#'   defined in the first metadata column.
#'
#' @return `data.frame`.
#' Contains 1:1 file to metadata sample identifier mappings.
#'
#' @examples
#' metadata <- data.frame(
#'     sampleID = paste(seq_len(4L), "sample", LETTERS[seq_len(4L)], sep = "_"),
#'     condition = rep(LETTERS[seq_len(2L)], times = 2L)
#' )
#' files <- file.path(
#'     "salmon",
#'     paste(seq_len(4L), "sample", LETTERS[seq_len(4L)], sep = "-"),
#'     "quant.sf"
#' )
#' ## The function will match regardless of row order in user metadata.
#' files <- rev(files)
#' print(metadata)
#' print(files)
#' matchMetadataToFiles(metadata = metadata, files = files)
matchMetadataToFiles <- function(metadata, files) {
    assert(
        is.data.frame(metadata),
        isCharacter(files)
    )
    metaSampleNames <- as.character(metadata[[1L]])
    fileSampleNames <- basename(dirname(files))
    assert(areSameLength(metaSampleNames, fileSampleNames))
    # Currently requiring that the user pass in tximport-style quant files.
    if (!areDisjointSets(fileSampleNames, ".")) {
        stop(
            "Failed to detect sample name from quant file.\n",
            "Example: 'salmon/sample-1/quant.sf'"
        )
    }
    idx <- match(
        x = snake(metaSampleNames),
        table = snake(fileSampleNames)
    )
    ## Return the sanitized name of file if defined.
    ## This gets slotted by `prepareTximportFiles()` return.
    if (hasNames(files)) {
        message("Returning 'names(files)' in data frame.")
        fileSampleNames <- names(files)
    }
    out <- data.frame(
        metadata = metaSampleNames,
        files = fileSampleNames[idx],
        stringsAsFactors = FALSE
    )
    if (!identical(anyNA(out, recursive = TRUE), FALSE)) {
        fail <- !complete.cases(out)
        fail <- out[fail, , drop = FALSE]
        stop("Match failure:\n", printString(fail))
    }
    assert(identical(
        x = as.character(metadata[[1L]]),
        y = as.character(out[[1L]])
    ))
    out
}
