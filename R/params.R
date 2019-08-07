#' @name params
#' @inherit acidroxygen::params
#' @keywords internal
#'
#' @param DESeqDataSet `DESeqDataSet` or `NULL`.
#' @param DESeqTransform `DESeqTransform`.
#' @param contrastSamples `logical(1)`.
#'   **Experimental**. Only include the samples used to define the contrast
#'   passed to [DESeq2::results()]. This setting will break for complex DESeq2
#'   contrasts (e.g. interaction effect).
#' @param lfcShrink `logical(1)`.
#'   Use shrunken log2 fold change (LFC) values.
#' @param results `character(1)` or `integer(1)`.
#'   Name or position of `DESeqResults`.
NULL
