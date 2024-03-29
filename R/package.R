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
#' SummarizedExperiment
NULL



## S4 generics and methods =====================================================

#' @importFrom AcidGenerics GeneToSymbol alphaSummary alphaThreshold
#' alphaThreshold<- as.DESeqDataSet as.DESeqTransform autopadZeros
#' baseMeanThreshold baseMeanThreshold<- camelCase contrastName contrastName<-
#' contrastSamples convertGenesToSymbols convertSampleIdsToNames correlation
#' deg degIntersection degPerContrast droplevels2 export interestingGroups
#' import interestingGroups<- intersectionMatrix leftJoin lfcShrink lfcShrink<-
#' lfcShrinkType lfcThreshold lfcThreshold<- makeNames mapGenesToSymbols
#' markdownTables melt metadata2 metadata2<- plotBaseMean plotContrastScatter
#' plotCorrelationHeatmap plotCounts plotDegHeatmap plotDegPca plotDegStackedBar
#' plotDegUpset plotHeatmap plotLfc plotMa plotPca plotQuantileHeatmap plotUpset
#' plotVolcano removeNa results resultsDiff resultsMatrix resultsNames
#' resultsNames<- resultsTables sampleData snakeCase transformType
#' @importFrom BiocGenerics Map Reduce anyDuplicated as.data.frame cbind
#' colnames combine counts design do.call match rownames sort unlist
#' updateObject
#' @importFrom DESeq2 priorInfo
#' @importFrom S4Vectors complete.cases cor decode droplevels head mcols mcols<-
#' metadata metadata<- na.omit split
#' @importFrom SummarizedExperiment assays assays<- colData colData<- rowData
#' @importFrom methods coerce show
NULL

#' @importMethodsFrom AcidBase intersectionMatrix
#' @importMethodsFrom AcidExperiment GeneToSymbol autopadZeros
#' convertGenesToSymbols convertSampleIdsToNames droplevels2 export
#' interestingGroups interestingGroups<- mapGenesToSymbols melt
#' @importMethodsFrom AcidGenomes GeneToSymbol
#' @importMethodsFrom AcidPlots plotCounts plotHeatmap plotPca plotUpset
#' @importMethodsFrom AcidPlyr leftJoin melt
#' @importMethodsFrom DESeq2 counts design
#' @importMethodsFrom pipette droplevels2 export import metadata2 metadata2<-
#' removeNa
#' @importMethodsFrom syntactic autopadZeros camelCase makeNames snakeCase
NULL



## S3 generics =================================================================

#' @importFrom stats model.matrix relevel
NULL



## Standard functions ==========================================================

#' @importFrom AcidBase initDir methodFunction printString realpath
#' showSlotInfo standardizeCall strMatch tempdir2 unlink2
#' @importFrom AcidCLI abort alert alertInfo alertWarning dl toInlineString
#' @importFrom AcidMarkdown markdownHeader markdownList
#' @importFrom AcidPlots .data acid_discrete_coord_flip acid_geom_label_repel
#' acid_scale_color_discrete acid_scale_fill_discrete matchLabels pretty_breaks
#' wrap_plots
#' @importFrom DESeq2 DESeq fpkm nbinomWaldTest
#' @importFrom IRanges DataFrameList SplitDataFrameList
#' @importFrom S4Vectors DataFrame SimpleList
#' @importFrom ggplot2 aes after_stat annotation_logticks element_blank geom_bar
#' geom_density geom_freqpoly geom_hline geom_point geom_text geom_vline ggplot
#' guides labs position_stack scale_color_manual scale_x_continuous
#' scale_y_continuous theme
#' @importFrom goalie allArePositive areDisjointSets areIntersectingSets
#' areSameLength areSetEqual assert bapply hasColnames hasDuplicates hasLength
#' hasNames hasLength hasRows hasRownames hasValidDimnames hasValidNames isADir
#' isAFile isAlpha isAny isCharacter isFlag isHeaderLevel isInRange isInt
#' isMatchingFixed isNegative isNonNegative isNumber isPercentage isPositive
#' isScalar isString isSubset isTximport requireNamespaces validate
#' @importFrom methods as is new prototype setClass setGeneric setValidity slot
#' slot<- validObject .hasSlot
#' @importFrom utils capture.output getS3method packageName packageVersion
NULL
