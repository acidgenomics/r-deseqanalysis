# Consider keeping `direction` exported in basejump.



#' @name params
#' @inherit basejump::params
#' @keywords internal
#'
#' @param alpha `numeric(1)`.
#'   Adjusted P value ("alpha") cutoff. If left `NULL`, will use the cutoff
#'   defined in the object.
#' @param contrastSamples `logical(1)`.
#'   **Experimental**. Only include the samples used to define the contrast
#'   passed to [DESeq2::results()]. This setting will break for complex DESeq2
#'   contrasts (e.g. interaction effect).
#' @param direction `character(1)`.
#'   Plot "`both`", "`up`", or "`down`" directions.
#' @param lfcShrink `logical(1)`.
#'   Use shrunken log2 fold change (LFC) values.
#' @param lfcThreshold `numeric(1)`.
#'   Log fold change ratio (base 2) cutoff threshold. If left `NULL`, will use
#'   the cutoff defined in the object.
#' @param results `character(1)` or `integer(1)`.
#'   Name or position of `DESeqResults`.
#' @param pointColor `character(1)`.
#'   Default point color for the plot.
#' @param sigPointColor `character`.
#'   Color names for labeling upregulated and downregulated genes. Also supports
#'   a character string for labeling DEGs with the same color, regardless of
#'   direction.
NULL
