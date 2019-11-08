#' Example DESeq2 differential expression analysis.
#' Updated 2019-07-30.

## FIXME The working example needs to include multiple contrasts.

library(pryr)
library(basejump)
library(DESeq2)
library(apeglm)

stopifnot(packageVersion("acidtest") >= "0.2.1")
data(RangedSummarizedExperiment, package = "acidtest")
rse <- RangedSummarizedExperiment

## Restrict to 2 MB.
## Use `pryr::object_size()` instead of `utils::object.size()`.
limit <- structure(2e6, class = "object_size")

## DESeqDataSet
## Consider updating the example RSE in basejump to include more genes.

## Requiring rich metadata to test `resultsTables()` and `topTables()`.
stopifnot(all(c("broadClass", "description") %in% colnames(rowData(rse))))

stopifnot("condition" %in% names(colData(rse)))
dds <- DESeqDataSet(se = rse, design = ~ condition)
dds <- DESeq(dds)
validObject(dds)

## DESeqTransform
dt <- varianceStabilizingTransformation(dds)

## DESeqResults
contrast <- resultsNames(dds)[[2L]]
res <- results(dds, name = contrast)
## Shrink log2 fold changes.
shrink <- lfcShrink(
    dds = dds,
    res = res,
    coef = contrast,
    type = "apeglm"
)

## Package up the analysis into a DESeqAnalysis object.
deseq <- DESeqAnalysis(
    data = dds,
    transform = dt,
    results = res,
    lfcShrink = shrink
)

## Check the object size.
lapply(coerceS4ToList(deseq), object_size)
object_size(deseq)
stopifnot(object_size(deseq) < limit)

## Check that object is valid.
stopifnot(is(deseq, "DESeqAnalysis"))
validObject(deseq)

usethis::use_data(deseq, overwrite = TRUE, compress = "xz")
