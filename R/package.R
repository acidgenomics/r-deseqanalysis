#' DESeqAnalysis
#'
#' Toolkit for performing differential expression with
#' [DESeq2](http://bioconductor.org/packages/DESeq2/).
#'
#' @aliases NULL
#' @keywords internal
#'
#' @importClassesFrom DESeq2 DESeqDataSet DESeqTransform
#' @importClassesFrom SummarizedExperiment RangedSummarizedExperiment
#'   SummarizedExperiment
#' @importClassesFrom basejump Tx2Gene
#'
#' @importMethodsFrom basejump coerce
#' 
#' @importFrom DESeq2 results resultsNames
#' @importFrom S4Vectors as.data.frame head mcols mcols<- metadata
#' @importFrom assertive.base assert_all_are_true assert_are_identical
#' @importFrom assertive.files assert_all_are_existing_files
#' @importFrom assertive.numbers assert_all_are_in_range
#'   assert_all_are_non_negative
#' @importFrom assertive.properties assert_has_names assert_is_non_empty
#'   assert_is_of_length assert_is_scalar has_names
#' @importFrom assertive.sets assert_are_disjoint_sets assert_is_subset
#' @importFrom assertive.types assert_is_a_bool assert_is_a_number
#'   assert_is_a_string assert_is_all_of assert_is_any_of assert_is_character
#'   assert_is_list assert_is_matrix assert_is_numeric is_a_string
#' @importFrom assertthat assert_that
#' @importFrom basejump Gene2Symbol assertFormalGene2Symbol as_tibble
#'   basejump_geom_label_repel camel humanize initDir interestingGroups<-
#'   mapGenesToRownames markdownHeader markdownList matchArgsToDoCall
#'   matchInterestingGroups plotGene plotHeatmap plotPCA sanitizeRowData
#'   showSlotInfo snake
#' @importFrom cowplot draw_plot ggdraw
#' @importFrom dplyr arrange desc everything filter left_join mutate pull rename
#'   row_number select
#' @importFrom ggplot2 aes annotation_logticks element_blank geom_density
#'   geom_hline geom_point geom_vline ggplot guides labs scale_color_manual
#'   scale_x_continuous scale_y_continuous theme
#' @importFrom goalie assertIsAlpha assertIsAnImplicitInteger
#'   assertIsHeaderLevel assertIsImplicitInteger assertIsStringOrNULL
#' @importFrom knitr kable
#' @importFrom magrittr %>%
#' @importFrom methods as is new slot slot<- validObject
#' @importFrom rlang := UQ has_length sym syms
#' @importFrom scales pretty_breaks
#' @importFrom stringr str_match str_trunc
#' @importFrom tidyselect starts_with
#' @importFrom utils capture.output globalVariables packageVersion
"_PACKAGE"



globalVariables(".")
