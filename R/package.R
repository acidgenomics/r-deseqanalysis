#' DESeqAnalysis
#'
#' Toolkit for performing differential expression with
#' [DESeq2](https://bioconductor.org/packages/DESeq2/).
#'
#' @aliases NULL
#' @keywords internal
"_PACKAGE"


## S4 classes ==================================================================

#' @importClassesFrom AcidBase missingOrNULL
#' @importClassesFrom AcidGenomes TxToGene
#' @importClassesFrom DESeq2 DESeqDataSet DESeqTransform
#' @importClassesFrom IRanges DFrameList
#' @importClassesFrom SummarizedExperiment RangedSummarizedExperiment
#' @importClassesFrom SummarizedExperiment SummarizedExperiment
NULL


## S4 generics and methods =====================================================

#' @importFrom AcidGenerics GeneToSymbol alphaSummary alphaThreshold
#' @importFrom AcidGenerics alphaThreshold<- as.DESeqDataSet as.DESeqTransform
#' @importFrom AcidGenerics autopadZeros baseMeanThreshold baseMeanThreshold<-
#' @importFrom AcidGenerics camelCase contrastName contrastName<-
#' @importFrom AcidGenerics contrastSamples convertGenesToSymbols
#' @importFrom AcidGenerics convertSampleIdsToNames correlation deg
#' @importFrom AcidGenerics degIntersection degPerContrast droplevels2 export
#' @importFrom AcidGenerics interestingGroups import interestingGroups<-
#' @importFrom AcidGenerics intersectionMatrix leftJoin lfcShrink lfcShrink<-
#' @importFrom AcidGenerics lfcShrinkType lfcThreshold lfcThreshold<-
#' @importFrom AcidGenerics makeNames mapGenesToSymbols markdownTables melt
#' @importFrom AcidGenerics metadata2 metadata2<- plotBaseMean
#' @importFrom AcidGenerics plotContrastScatter plotCorrelationHeatmap
#' @importFrom AcidGenerics plotCounts plotDegHeatmap plotDegPca
#' @importFrom AcidGenerics plotDegStackedBar plotDegUpset plotHeatmap plotLfc
#' @importFrom AcidGenerics plotMa plotPca plotQuantileHeatmap plotUpset
#' @importFrom AcidGenerics plotVolcano removeNa results resultsDiff
#' @importFrom AcidGenerics resultsMatrix resultsNames resultsNames<-
#' @importFrom AcidGenerics resultsTables sampleData snakeCase transformType
#' @importFrom BiocGenerics Map Reduce anyDuplicated as.data.frame cbind
#' @importFrom BiocGenerics colnames combine counts design do.call match
#' @importFrom BiocGenerics rownames sort unlist updateObject
#' @importFrom DESeq2 priorInfo
#' @importFrom S4Vectors complete.cases cor decode droplevels head mcols
#' @importFrom S4Vectors mcols<- metadata metadata<- na.omit split
#' @importFrom SummarizedExperiment assays assays<- colData colData<- rowData
#' @importFrom methods coerce show
NULL

#' @importMethodsFrom AcidBase intersectionMatrix
#' @importMethodsFrom AcidExperiment GeneToSymbol autopadZeros
#' @importMethodsFrom AcidExperiment convertGenesToSymbols
#' @importMethodsFrom AcidExperiment convertSampleIdsToNames droplevels2
#' @importMethodsFrom AcidExperiment export interestingGroups
#' @importMethodsFrom AcidExperiment interestingGroups<- mapGenesToSymbols melt
#' @importMethodsFrom AcidGenomes GeneToSymbol
#' @importMethodsFrom AcidPlots plotCounts plotHeatmap plotPca plotUpset
#' @importMethodsFrom AcidPlyr leftJoin melt
#' @importMethodsFrom DESeq2 counts design
#' @importMethodsFrom pipette droplevels2 export import metadata2 metadata2<-
#' @importMethodsFrom pipette removeNa
#' @importMethodsFrom syntactic autopadZeros camelCase makeNames snakeCase
NULL


## S3 generics =================================================================

#' @importFrom stats model.matrix relevel
NULL


## Standard functions ==========================================================

#' @importFrom AcidBase initDir methodFunction printString realpath
#' @importFrom AcidBase showSlotInfo standardizeCall strMatch tempdir2 unlink2
#' @importFrom AcidCLI abort alert alertInfo alertWarning dl toInlineString
#' @importFrom AcidMarkdown markdownHeader markdownList
#' @importFrom AcidPlots .data acid_discrete_coord_flip acid_geom_label_repel
#' @importFrom AcidPlots acid_scale_color_discrete acid_scale_fill_discrete
#' @importFrom AcidPlots matchLabels pretty_breaks wrap_plots
#' @importFrom DESeq2 DESeq fpkm nbinomWaldTest
#' @importFrom IRanges DataFrameList SplitDataFrameList
#' @importFrom S4Vectors DataFrame SimpleList
#' @importFrom ggplot2 aes after_stat annotation_logticks element_blank
#' @importFrom ggplot2 geom_bar geom_density geom_freqpoly geom_hline
#' @importFrom ggplot2 geom_point geom_text geom_vline ggplot guides labs
#' @importFrom ggplot2 position_stack scale_color_manual scale_x_continuous
#' @importFrom ggplot2 scale_y_continuous theme
#' @importFrom goalie allArePositive areDisjointSets areIntersectingSets
#' @importFrom goalie areSameLength areSetEqual assert bapply hasColnames
#' @importFrom goalie hasDuplicates hasLength hasNames hasRows hasRownames
#' @importFrom goalie hasValidDimnames hasValidNames isADir isAFile isAlpha
#' @importFrom goalie isAny isCharacter isFlag isHeaderLevel isInRange isInt
#' @importFrom goalie isMatchingFixed isNegative isNonNegative isNumber
#' @importFrom goalie isPercentage isPositive isScalar isString isSubset
#' @importFrom goalie isTximport requireNamespaces validate
#' @importFrom methods as is new prototype setClass setGeneric setValidity
#' @importFrom methods slot slot<- validObject .hasSlot
#' @importFrom utils capture.output getS3method packageName packageVersion
NULL
