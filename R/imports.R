#' @importClassesFrom DESeq2 DESeqDataSet DESeqTransform
#' @importClassesFrom SummarizedExperiment RangedSummarizedExperiment
#'   SummarizedExperiment
#' @importClassesFrom basejump Tx2Gene
#'
#' @importMethodsFrom basejump coerce
#'
#' @importFrom BiocGenerics updateObject
#' @importFrom DESeq2 counts priorInfo
#' @importFrom IRanges DataFrameList
#' @importFrom S4Vectors SimpleList as.data.frame head mcols mcols<- metadata
#' @importFrom SummarizedExperiment assays assays<- colData colData<- rowData
#' @importFrom UpSetR fromList
#' @importFrom acidplots plotHeatmap scale_colour_synesthesia_d upset
#' @importFrom basejump Gene2Symbol as_tibble camelCase convertSampleIDsToNames
#'   decode initDir interestingGroups<- makeNames mapGenesToRownames
#'   markdownHeader markdownList matchArgsToDoCall matchInterestingGroups
#'   matchesGene2Symbol methodFormals printString relevel removeNA showSlotInfo
#'   standardizeCall
#' @importFrom cowplot draw_plot ggdraw
#' @importFrom dplyr arrange desc everything filter left_join mutate mutate_all
#'   pull rename row_number select
#' @importFrom ggplot2 aes annotation_logticks element_blank geom_density
#'   geom_hline geom_point geom_vline ggplot guides labs scale_colour_manual
#'   scale_x_continuous scale_y_continuous theme
#' @importFrom goalie areDisjointSets areSetEqual assert bapply hasLength
#'   hasNames hasLength hasRows hasRownames hasValidDimnames hasValidNames
#'   isAlpha isAny isCharacter isFlag isHeaderLevel isInRange isInt isNonEmpty
#'   isNonNegative isNumber isPercentage isPositive isScalar isString isSubset
#'   validate
#' @importFrom knitr kable
#' @importFrom magrittr %>%
#' @importFrom methods as is new prototype setClass show slot slot<- validObject
#'   .hasSlot
#' @importFrom acidplots acid_geom_label_repel plotCounts plotPCA
#' @importFrom rlang !! := UQ sym syms
#' @importFrom scales log_breaks pretty_breaks
#' @importFrom stringr str_match str_trunc
#' @importFrom tidyselect starts_with
#' @importFrom utils capture.output getS3method globalVariables packageVersion
NULL

## Note that `summary.DESeqResults()` isn't currently exported in BioC 3.10.
