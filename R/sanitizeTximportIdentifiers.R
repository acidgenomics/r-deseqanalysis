#' Sanitize tximport identifiers
#'
#' @export
#' @note Updated 2023-07-25.
#'
#' @details
#' Fix transcript identifiers that contain `"|"` delimters. This can occur when
#' importing GENCODE-aligned data from kallisto, and BAM files from salmon.
#'
#' No modification occurs for objects not containing these types of identifier.
#'
#' @param txi `list`.
#' tximport list.
#'
#' @return `list`.
#' tximport list with corrected transcript identifiers.
#'
#' @examples
#' suppressPackageStartupMessages({
#'     library(tximport)
#'     library(tximportData)
#' })
#' dir <- system.file("extdata", package = "tximportData")
#' samples <- read.table(file.path(dir, "samples.txt"), header = TRUE)
#' files <- file.path(dir, "salmon", samples[["run"]], "quant.sf.gz")
#' names(files) <- paste0("sample", seq(from = 1L, to = length(files)))
#' txi <- tximport(files, type = "salmon", txIn = TRUE, txOut = TRUE)
#' txi <- sanitizeTximportIdentifiers(txi)
#' print(head(rownames(txi[["counts"]])))
sanitizeTximportIdentifiers <- function(txi) {
    assert(isTximport(txi))
    if (!all(grepl(
        pattern = "|",
        x = rownames(txi[["abundance"]]),
        fixed = TRUE
    ))) {
        return(txi)
    }
    sanitize <- function(x) {
        sub(
            pattern = "^([^|]+)\\|.+$",
            replacement = "\\1",
            x = x
        )
    }
    for (slot in c("abundance", "counts", "length")) {
        rownames(txi[[slot]]) <- sanitize(rownames(txi[[slot]]))
    }
    txi
}
