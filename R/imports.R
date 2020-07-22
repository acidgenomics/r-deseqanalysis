## FIXME Remove UpSetR `fromList` dependency here.



#' @importClassesFrom DESeq2 DESeqDataSet DESeqTransform
#' @importClassesFrom SummarizedExperiment RangedSummarizedExperiment
#'   SummarizedExperiment
#' @importClassesFrom basejump Tx2Gene
#'
#' @importMethodsFrom basejump coerce
#'
#' @importFrom BiocGenerics updateObject
#' @importFrom DESeq2 DESeq counts design fpkm lfcShrink nbinomWaldTest
#'   priorInfo
#' @importFrom IRanges DataFrameList
#' @importFrom S4Vectors DataFrame SimpleList as.data.frame cbind complete.cases
#'   cor do.call head mcols mcols<- metadata metadata<- na.omit
#' @importFrom SummarizedExperiment assays assays<- colData colData<- rowData
#' @importFrom UpSetR fromList
#' @importFrom acidplots acid_coord_flip matchLabels plotHeatmap
#'   scale_color_synesthesia_d plotUpset
#' @importFrom basejump Gene2Symbol as_tibble autopadZeros camelCase
#'   convertSampleIDsToNames decode droplevels import initDir
#'   interestingGroups<- leftJoin makeNames mapGenesToRownames markdownHeader
#'   markdownList matchInterestingGroups matchesGene2Symbol melt metadata2
#'   metadata2<- printString realpath removeNA showSlotInfo snakeCase
#'   standardizeCall
#' @importFrom cowplot draw_plot ggdraw
#' @importFrom ggplot2 aes annotation_logticks element_blank geom_bar
#'   geom_density geom_hline geom_point geom_text geom_vline ggplot guides labs
#'   position_stack scale_color_manual scale_x_continuous scale_y_continuous
#'   theme
#' @importFrom goalie areDisjointSets areIntersectingSets areSameLength
#'   areSetEqual assert bapply hasLength hasNames hasLength hasRows hasRownames
#'   hasValidDimnames hasValidNames isAFile isAlpha isAny isCharacter isFlag
#'   isGGScale isHeaderLevel isInRange isInt isNonNegative isNumber isPercentage
#'   isPositive isScalar isString isSubset validate
#' @importFrom knitr kable
#' @importFrom methods as is new prototype setClass setValidity show slot slot<-
#'   validObject .hasSlot
#' @importFrom acidplots acid_geom_label_repel plotCounts plotPCA
#' @importFrom rlang !! sym
#' @importFrom scales log_breaks pretty_breaks
#' @importFrom stats model.matrix relevel
#' @importFrom stringr str_locate_all str_match str_trunc
#' @importFrom utils capture.output getS3method globalVariables packageVersion
NULL
