#' Example DESeq2 differential expression analysis
#' 2018-11-19

library(pryr)
library(basejump)
library(DESeq2)

# Restrict to 2 MB.
# Use `pryr::object_size()` instead of `utils::object.size()`.
limit <- structure(2e6, class = "object_size")

# DESeqDataSet
# Consider having the example RSE in basejump include more genes.
data(rse, package = "basejump")
dds <- DESeqDataSet(se = rse, design = ~ treatment)
dds <- DESeq(dds)
validObject(dds)

# DESeqTransform
dt <- varianceStabilizingTransformation(dds)

# DESeqResults
contrast <- resultsNames(dds)[[2L]]
res <- results(dds, name = contrast)

# Shrink log2 fold changes
res_shrunken <- lfcShrink(dds = dds, coef = contrast, res = res)
validObject(res_shrunken)

deseq <- DESeqAnalysis(
    data = dds,
    transform = dt,
    results = list(res),
    lfcShrink = list(res_shrunken)
)
validObject(deseq)
print(deseq)

# Report the size of each slot in bytes.
vapply(
    X = coerceS4ToList(deseq),
    FUN = object_size,
    FUN.VALUE = numeric(1L)
)
object_size(deseq)
stopifnot(object_size(bcb) < limit)

# Check that object is valid.
stopifnot(is(deseq, "DESeqAnalysis"))
stopifnot(validObject(deseq))

usethis::use_data(deseq, overwrite = TRUE, compress = "xz")
