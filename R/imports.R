#' @importClassesFrom DESeq2 DESeqDataSet DESeqTransform
#' @importClassesFrom SummarizedExperiment RangedSummarizedExperiment
#'   SummarizedExperiment
#' @importClassesFrom basejump Tx2Gene
#'
#' @importMethodsFrom basejump coerce
#'
#' @importFrom DESeq2 results resultsNames
#' @importFrom IRanges DataFrameList
#' @importFrom S4Vectors SimpleList as.data.frame head mcols mcols<- metadata
#' @importFrom SummarizedExperiment colData colData<-
#' @importFrom basejump Gene2Symbol as_tibble basejump_geom_label_repel camel
#'   convertSampleIDsToNames decode humanize initDir interestingGroups<-
#'   makeNames mapGenesToRownames markdownHeader markdownList matchArgsToDoCall
#'   matchInterestingGroups matchesGene2Symbol methodFormals plotGene
#'   plotHeatmap plotPCA relevelColData removeNA showSlotInfo snake
#'   standardizeCall
#' @importFrom cowplot draw_plot ggdraw
#' @importFrom dplyr arrange desc everything filter left_join mutate mutate_all
#'   pull rename row_number select
#' @importFrom ggplot2 aes annotation_logticks element_blank geom_density
#'   geom_hline geom_point geom_vline ggplot guides labs scale_color_manual
#'   scale_x_continuous scale_y_continuous theme
#' @importFrom goalie areDisjointSets assert bapply hasLength hasNames hasLength
#'   hasRows hasRownames hasValidDimnames isAlpha isAny isCharacter isFlag
#'   isHeaderLevel isInRange isInt isNonEmpty isNonNegative isNumber isPositive
#'   isScalar isString isSubset validate
#' @importFrom knitr kable
#' @importFrom magrittr %>%
#' @importFrom methods as is new prototype setClass slot slot<- validObject
#' @importFrom rlang := UQ sym syms
#' @importFrom scales pretty_breaks
#' @importFrom stringr str_match str_trunc
#' @importFrom tidyselect starts_with
#' @importFrom utils capture.output globalVariables packageVersion
NULL
