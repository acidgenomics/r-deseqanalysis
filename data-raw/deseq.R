#' Example DESeq2 differential expression analysis.
#' Updated 2019-11-12.

library(usethis)   # 1.5.1
library(pryr)      # 0.1.4
library(basejump)  # 0.11.22
library(DESeq2)    # 1.26.0
library(apeglm)    # 1.8.0

stopifnot(packageVersion("acidtest") >= "0.2.8")
data(RangedSummarizedExperiment, package = "acidtest")
rse <- RangedSummarizedExperiment

## Restrict to 2 MB.
## Use `pryr::object_size()` instead of `utils::object.size()`.
limit <- structure(2e6, class = "object_size")

## Requiring rich metadata to test `resultsTables()` and `topTables()`.
stopifnot(
    all(c("broadClass", "description") %in% colnames(rowData(rse))),
    "condition" %in% names(colData(rse))
)

## DESeqDataSet
rse$treatment <- as.factor(x = rep(c("C", "D"), each = 3L))
design <- ~ condition + treatment + condition:treatment
dds <- DESeqDataSet(se = rse, design = design)
dds <- DESeq(dds)
validObject(dds)

## DESeqTransform
dt <- varianceStabilizingTransformation(dds)

## DESeqResults list
alpha <- 0.01
lfcThreshold <- 0L
contrasts <- list(
    "condition_B_vs_A" = c(
        factor = "condition",
        numerator = "B",
        denominator = "A"
    ),
    "treatment_D_vs_C" = c(
        factor = "treatment",
        numerator = "D",
        denominator = "C"
    )
)
res <- mapply(
    FUN = DESeq2::results,
    contrast = contrasts,
    MoreArgs = list(
        object = dds,
        lfcThreshold = lfcThreshold,
        alpha = alpha
    ),
    SIMPLIFY = FALSE,
    USE.NAMES = TRUE
)

## Shrink log2 fold changes via `DESeq2::lfcShrink()`.
## apeglm is now recommended over normal. Also note that LFC shrinkage via
## type='normal' is not implemented for designs with interactions.
shrink <- mapply(
    FUN = apeglmContrast,
    contrast = contrasts,
    res = res,
    MoreArgs = list(
        dds = dds,
        lfcThreshold = lfcThreshold
    ),
    SIMPLIFY = FALSE,
    USE.NAMES = TRUE
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

use_data(deseq, overwrite = TRUE, compress = "xz")
