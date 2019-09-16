## Note that `summary.DESeqResults()` isn't currently exported in BioC 3.10.



#' @importClassesFrom DESeq2 DESeqDataSet DESeqTransform
#' @importClassesFrom SummarizedExperiment RangedSummarizedExperiment
#'   SummarizedExperiment
#' @importClassesFrom basejump Tx2Gene
#'
#' @importMethodsFrom basejump coerce
#'
#' @importFrom BiocGenerics updateObject
#' @importFrom DESeq2 counts fpkm priorInfo
#' @importFrom IRanges DataFrameList
#' @importFrom S4Vectors SimpleList as.data.frame cbind do.call head mcols
#'   mcols<- metadata metadata<- na.omit
#' @importFrom SummarizedExperiment assays assays<- colData colData<- rowData
#' @importFrom UpSetR fromList
#' @importFrom acidplots plotHeatmap scale_color_synesthesia_d upset
#' @importFrom basejump Gene2Symbol as_tibble camelCase convertSampleIDsToNames
#'   decode droplevels import initDir interestingGroups<- leftJoin makeNames
#'   mapGenesToRownames markdownHeader markdownList matchArgsToDoCall
#'   matchInterestingGroups matchesGene2Symbol methodFormals printString
#'   removeNA showSlotInfo snakeCase standardizeCall
#' @importFrom cowplot draw_plot ggdraw
#' @importFrom ggplot2 aes annotation_logticks element_blank geom_density
#'   geom_hline geom_point geom_vline ggplot guides labs scale_color_manual
#'   scale_x_continuous scale_y_continuous theme
#' @importFrom goalie areDisjointSets areSetEqual assert bapply hasLength
#'   hasNames hasLength hasRows hasRownames hasValidDimnames hasValidNames
#'   isAFile isAlpha isAny isCharacter isFlag isHeaderLevel isInRange isInt
#'   isNonEmpty isNonNegative isNumber isPercentage isPositive isScalar isString
#'   isSubset validate
#' @importFrom knitr kable
#' @importFrom methods as is new prototype setClass show slot slot<- validObject
#'   .hasSlot
#' @importFrom acidplots acid_geom_label_repel plotCounts plotPCA
#' @importFrom rlang !! sym
#' @importFrom scales log_breaks pretty_breaks
#' @importFrom stringr str_match str_trunc
#' @importFrom utils capture.output getS3method globalVariables packageVersion
NULL
