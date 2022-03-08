#' @name params
#' @inherit AcidRoxygen::params return title
#' @keywords internal
#'
#' @param DESeqDataSet `DESeqDataSet` or `NULL`.
#' @param DESeqTransform `DESeqTransform`.
#' @param contrast `character`.
#'   A character vector with exactly 3 elements:
#'
#'   1. Name of factor in the design formula.
#'   2. Name of numerator level for the fold change.
#'   3. Name of denominator level for the fold change.
#'
#'   See `DESeq2::results()` for details.
#' @param contrastSamples `logical(1)`.
#'   Only include the samples used to define the contrast passed to
#'   `DESeq2::results()`. This setting will break for complex DESeq2 contrasts
#'   (e.g. interaction effect).
#' @param i `character(1)` or `integer(1)`.
#'   `DESeqResults` contrast name or position in `results` slot.
#' @param lfcShrink `logical(1)` or `NULL`.
#'   Use shrunken log2 fold change (LFC) values.
#'   If `NULL`, inherits value defined in `lfcShrink()`.
#' @param limits `list(2)`.
#'   Named list containing `"x"` and `"y"` that define the lower and upper
#'   limits for each axis. Set automatically by default when left `NULL`.
#' @param name `character(1)`.
#'   Name of the individual effect (coefficient) for building a results table.
#'   Use this argument rather than `contrast` for continuous variables.
#' @param results `character(1)` or `integer(1)`.
#'   Name or position of `DESeqResults`.
NULL
