## nolint start
suppressPackageStartupMessages({
    library(devtools)
    library(usethis)
})
## nolint end
load_all()
data(RangedSummarizedExperiment, package = "AcidTest")
rse <- RangedSummarizedExperiment
## Restrict to 2 MB.
## Use `pryr::object_size()` instead of `utils::object.size()`.
limit <- structure(2e6L, class = "object_size")
## Requiring rich metadata to test `resultsTables()` and `topTables()`.
stopifnot(
    all(c("broadClass", "description") %in% colnames(rowData(rse))),
    "condition" %in% names(colData(rse))
)
## Generate DESeqDataSet.
colData(rse)[["treatment"]] <- as.factor(x = rep(c("C", "D"), each = 3L))
design <- ~ condition + treatment + condition:treatment
dds <- DESeqDataSet(se = rse, design = design)
dds <- DESeq(dds)
validObject(dds)
## Generate DESeqTransform.
dt <- varianceStabilizingTransformation(dds)
## Generate DESeqResults list.
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
res <- Map(
    f = DESeq2::results,
    contrast = contrasts,
    MoreArgs = list(
        "object" = dds,
        "lfcThreshold" = lfcThreshold,
        "alpha" = alpha
    )
)
names(res) <- names(contrasts)
## Shrink log2 fold changes via `DESeq2::lfcShrink()`. apeglm is now recommended
## over normal. Also note that LFC shrinkage via type='normal' is not
## implemented for designs with interactions.
shrink <- Map(
    f = apeglmResults,
    contrast = contrasts,
    res = res,
    MoreArgs = list(
        "object" = dds,
        "lfcThreshold" = lfcThreshold
    )
)
names(shrink) <- names(contrasts)
## Package up the analysis into a DESeqAnalysis object.
deseq <- DESeqAnalysis(
    data = dds,
    transform = dt,
    results = res,
    lfcShrink = shrink
)
## Check the object size.
stopifnot(
    is(deseq, "DESeqAnalysis"),
    validObject(deseq),
    object.size(deseq) < limit
)
use_data(deseq, overwrite = TRUE, compress = "xz")
