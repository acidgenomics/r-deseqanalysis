#' Sanitize tximport identifiers
#'
#' @export
#' @note Updated 2023-07-25.
#'
#' @details
#' Fix transcript identifiers that contain `"|"` delimters. This can occur
#' when importing GENCODE-aligned data from kallisto, and BAM files from salmon.
#'
#' No modification occurs for objects not containing these types of identifier.
#'
#' @param txi `list`.
#' tximport list.
#'
#' @return `list`.
#' tximport list with corrected identifiers.
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
#' tx2gene <- read.csv(file.path(dir, "tx2gene.gencode.v27.csv"))
#' txi <- tximport(files, type = "salmon", tx2gene = tx2gene)
#' txi <- sanitizeTximportIdentifiers
#' print(dimnames(txi[["counts"]][1L:5L, 1L:5L]))
sanitizeTximportIdentifiers <- function(txi) {
    assert(isTximport(txi))
}
