#' Match user metadata to file names used for tximport
#'
#' @export
#' @note Updated 2019-10-09.
#'
#' @param files `character`.
#'   Quant file paths passed to [`tximport()`][tximport::tximport].
#' @param metadata `data.frame`.
#'   User-defined metadata. The function assumes that sample identifiers are
#'   defined in the first metadata column.
#'
#' @return `data.frame`.
#' Contains 1:1 file to metadata sample identifier mappings.
#'
#' @examples
#' files <- file.path(
#'     "salmon",
#'     paste(seq_len(4L), "sample", LETTERS[seq_len(4L)], sep = "-"),
#'     "quant.sf"
#' )
#' metadata <- data.frame(
#'     sampleID = paste(seq_len(4L), "sample", LETTERS[seq_len(4L)], sep = "_"),
#'     condition = rep(LETTERS[seq_len(2L)], times = 2L)
#' )
#' ## The function will match regardless of row order in user metadata.
#' metadata <- metadata[nrow(metadata):1L, ]
#' print(files)
#' print(metadata)
#' matchFilesToMetadata(files = files, metadata = metadata)
matchFilesToMetadata <- function(files, metadata) {
    assert(
        isCharacter(files),
        is.data.frame(metadata)
    )
    sampleNames <- basename(dirname(files))
    # Currently requiring that the user pass in tximport-style quant files.
    if (!areDisjointSets(sampleNames, ".")) {
        stop(
            "Failed to detect sample name from quant file.\n",
            "Example: 'salmon/sample-1/quant.sf'"
        )
    }
    input <- list(
        files = sampleNames,
        metadata = metadata[[1L]]
    )
    idx <- match(
        x = snake(input[["files"]]),
        table = snake(input[["metadata"]])
    )
    output <- data.frame(
        files = input[["files"]],
        metadata = input[["metadata"]][idx]
    )
    if (!identical(anyNA(output, recursive = TRUE), FALSE)) {
        fail <- !complete.cases(output)
        fail <- output[fail, , drop = FALSE]
        stop("Match failure:\n", printString(fail))
    }
    output
}
