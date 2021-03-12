#' DESeqAnalysis
#'
#' Toolkit for performing differential expression with
#' [DESeq2](http://bioconductor.org/packages/DESeq2/).
#'
#' @aliases NULL
#' @keywords internal
#'
#' @importClassesFrom DESeq2 DESeqDataSet DESeqTransform
#' @importClassesFrom basejump DataFrameList RangedSummarizedExperiment
#'   SummarizedExperiment Tx2Gene missingOrNULL
#'
#' @importMethodsFrom basejump coerce
#'
#' @importFrom DESeq2 DESeq counts design fpkm nbinomWaldTest priorInfo
#' @importFrom AcidPlots !! acid_coord_flip acid_geom_label_repel draw_plot
#'   ggdraw matchLabels plotHeatmap pretty_breaks scale_color_synesthesia_d
#'   plotCounts plotPCA plotUpset sym
#' @importFrom basejump DataFrame DataFrameList Gene2Symbol SimpleList alert
#'   alertInfo alertWarning as.data.frame as_tibble assays assays<- autopadZeros
#'   camelCase capture.output cbind colData colData<- complete.cases
#'   convertSampleIDsToNames cor decode dl do.call droplevels formalsList
#'   getS3method head import initDir interestingGroups<- intersectionMatrix
#'   leftJoin makeNames mapGenesToRownames markdownHeader markdownList
#'   matchesGene2Symbol mcols mcols<- melt metadata metadata<- metadata2
#'   metadata2<- model.matrix na.omit packageName packageVersion printString
#'   realpath relevel removeNA requireNamespaces rowData showSlotInfo snakeCase
#'   standardizeCall
#' @importFrom ggplot2 aes annotation_logticks element_blank geom_bar
#'   geom_density geom_hline geom_point geom_text geom_vline ggplot guides labs
#'   position_stack scale_color_manual scale_x_continuous scale_y_continuous
#'   theme
#' @importFrom goalie areDisjointSets areIntersectingSets areSameLength
#'   areSetEqual assert bapply hasLength hasNames hasLength hasRows hasRownames
#'   hasValidDimnames hasValidNames isAFile isAlpha isAny isCharacter isFlag
#'   isGGScale isHeaderLevel isInRange isInt isNonNegative isNumber isPercentage
#'   isPositive isScalar isString isSubset validate
#' @importFrom methods as is new prototype setClass setGeneric setValidity show
#'   slot slot<- validObject .hasSlot
#' @importFrom stringr str_locate_all str_match str_trunc
"_PACKAGE"
