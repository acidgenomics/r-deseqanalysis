#' Match user metadata to file names used for tximport
#'
#' @export
#' @note Updated 2021-06-28.
#'
#' @param metadata `data.frame`.
#'   User-defined metadata. The function assumes that sample identifiers are
#'   defined in the first metadata column.
#' @param files `character`.
#'   Quant file paths passed to `tximport::tximport()`.
#'   Sanitize return from `prepareTximportFiles()` is recommended.
#'
#' @return `data.frame`.
#' Modified metadata frame, with updated sample identifiers in slotted in
#' first column. Original values are stashed in "originalSampleId".
#'
#' @examples
#' metadata <- data.frame(
#'     "sampleId" = paste(
#'         seq_len(4L), "sample", LETTERS[seq_len(4L)],
#'         sep = "_"
#'     ),
#'     "condition" = rep(LETTERS[seq_len(2L)], times = 2L)
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
        areDisjointSets("originalSampleId", colnames(metadata)),
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
        x = snakeCase(metaSampleNames),
        table = snakeCase(fileSampleNames)
    )
    ## Return the sanitized name of file if defined.
    ## This gets slotted by `prepareTximportFiles()` return.
    if (hasNames(files)) {
        alert("Returning {.var names(files)} in data frame.")
        fileSampleNames <- names(files)
    }
    map <- data.frame(
        metadata = metaSampleNames,
        files = fileSampleNames[idx],
        stringsAsFactors = FALSE
    )
    if (!identical(anyNA(map, recursive = TRUE), FALSE)) {
        fail <- !complete.cases(map)
        fail <- map[fail, , drop = FALSE]
        stop("Match failure:\n", printString(fail))
    }
    assert(identical(
        x = as.character(metadata[[1L]]),
        y = as.character(map[[1L]])
    ))
    out <- metadata
    out[[1L]] <- map[["files"]]
    out[["originalSampleId"]] <- metadata[[1L]]
    out
}
