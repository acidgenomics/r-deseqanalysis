#' @name params
#' @inherit basejump::params
#' @keywords internal
#' @param contrastSamples `logical(1)`. **Experimental**. Only include the samples
#'   used to define the contrast passed to `DESeq2::results`. This setting
#'   will break for complex DESeq2 contrasts (e.g. interaction effect).
#' @param lfcShrink `logical(1)`. Use shrunken log2 fold change (LFC) values.
#' @param results `character(1)` or `integer(1)`. Name or positive of
#'   `DESeqResults`.
NULL
