#' Example DESeq2 differential expression analysis
#' 2019-02-06

library(pryr)
library(basejump)
library(DESeq2)

data(rse, package = "basejump")

# Restrict to 2 MB.
# Use `pryr::object_size` instead of `utils::object.size`.
limit <- structure(2e6, class = "object_size")

# DESeqDataSet
# Consider updating the example RSE in basejump to include more genes.
dds <- DESeqDataSet(se = rse, design = ~ condition)
dds <- DESeq(dds)
validObject(dds)

# DESeqTransform
dt <- varianceStabilizingTransformation(dds)

# DESeqResults
contrast <- resultsNames(dds)[[2L]]
res <- results(dds, name = contrast)
# Shrink log2 fold changes.
shrink <- lfcShrink(
    dds = dds,
    res = res,
    coef = contrast,
    type = "apeglm"
)

# Package up the analysis into a DESeqAnalysis object.
deseq <- DESeqAnalysis(
    data = dds,
    transform = dt,
    results = res,
    lfcShrink = shrink
)

# Report the size of each slot in bytes.
vapply(
    X = coerceS4ToList(deseq),
    FUN = object_size,
    FUN.VALUE = numeric(1L)
)
object_size(deseq)
stopifnot(object_size(deseq) < limit)

# Check that object is valid.
stopifnot(is(deseq, "DESeqAnalysis"))
validObject(deseq)

usethis::use_data(deseq, overwrite = TRUE, compress = "xz")
