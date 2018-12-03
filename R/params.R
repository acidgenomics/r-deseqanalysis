#' @name params
#' @inherit basejump::params
#' @keywords internal
#' @param contrastSamples `boolean`. **Experimental**. Only include the samples
#'   used to define the contrast passed to `DESeq2::results()`. This setting
#'   will break for complex DESeq2 contrasts (e.g. interaction effect).
#' @param lfcShrink `boolean`. Use shrunken log2 fold change (LFC) values.
#' @param results `scalar`. Position or name of `DESeqResults`.
NULL
