## nolint start
suppressPackageStartupMessages({
    library(devtools)
    library(usethis)
    library(AcidGenomes)
})
## nolint end
load_all()
dds <- makeExampleDESeqDataSet(n = 500L, m = 12L, betaSD = 1L)
rowRanges <- makeGRangesFromEnsembl(
    organism = "Homo sapiens",
    level = "genes",
    release = 100L,
    ignoreVersion = FALSE
)
rowRanges <- as(rowRanges, "GRanges")
rowRanges <- rowRanges[sort(names(rowRanges))]
rowRanges <- rowRanges[seq_len(nrow(dds)), ]
rowRanges <- droplevels2(rowRanges)
names(rowRanges) <- rownames(dds)
rowRanges(dds) <- rowRanges
stopifnot("condition" %in% names(colData(dds)))
colData(dds)[["treatment"]] <- as.factor(x = rep(c("C", "D"), each = 3L))
design(dds) <- ~ condition + treatment + condition:treatment
dds <- DESeq(dds)
dt <- varianceStabilizingTransformation(dds)
alpha <- 0.01
lfcThreshold <- 0L
contrasts <- list(
    "condition_B_vs_A" = c(
        "factor" = "condition",
        "numerator" = "B",
        "denominator" = "A"
    ),
    "treatment_D_vs_C" = c(
        "factor" = "treatment",
        "numerator" = "D",
        "denominator" = "C"
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
shrink <- Map(
    f = apeglmResults,
    contrast = contrasts,
    res = res,
    MoreArgs = list(
        "object" = dds,
        "lfcThreshold" = lfcThreshold
    )
)
deseq <- DESeqAnalysis(
    data = dds,
    transform = dt,
    results = res,
    lfcShrink = shrink
)
limit <- structure(2e6L, class = "object_size")
stopifnot(
    is(deseq, "DESeqAnalysis"),
    validObject(deseq),
    object.size(deseq) < limit
)
use_data(deseq, overwrite = TRUE, compress = "xz")
