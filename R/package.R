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
#'   convertGenesToSymbols convertSampleIDsToNames cor decode dl do.call
#'   droplevels formalsList getS3method head import initDir interestingGroups<-
#'   intersectionMatrix leftJoin makeNames mapGenesToSymbols markdownHeader
#'   markdownList matchesGene2Symbol mcols mcols<- melt metadata metadata<-
#'   metadata2 metadata2<- model.matrix na.omit packageName packageVersion
#'   printString realpath relevel removeNA requireNamespaces rowData
#'   showSlotInfo snakeCase standardizeCall str_locate_all str_match str_trunc
#' @importFrom ggplot2 aes annotation_logticks element_blank geom_bar
#'   geom_density geom_hline geom_point geom_text geom_vline ggplot guides labs
#'   position_stack scale_color_manual scale_x_continuous scale_y_continuous
#'   theme
#' @importFrom goalie allArePositive areDisjointSets areIntersectingSets
#'   areSameLength areSetEqual assert bapply hasLength hasNames hasLength
#'   hasRows hasRownames hasValidDimnames hasValidNames isAFile isAlpha isAny
#'   isCharacter isFlag isGGScale isHeaderLevel isInRange isInt isNegative
#'   isNonNegative isNumber isPercentage isPositive isScalar isString isSubset
#'   validate
#' @importFrom methods as is new prototype setClass setGeneric setValidity show
#'   slot slot<- validObject .hasSlot
"_PACKAGE"



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
