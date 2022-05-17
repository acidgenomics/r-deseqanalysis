## nolint start
suppressPackageStartupMessages({
    library(devtools)
    library(usethis)
})
## nolint end
load_all()
limit <- structure(2e6L, class = "object_size")
dds <- makeExampleDESeqDataSet()
dds <- DESeq(dds)
dt <- varianceStabilizingTransformation(dds)
res <- results(dds)
object <- DESeqAnalysis(
    data = dds,
    transform = dt,
    results = res,
    lfcShrink = NULL
)
stopifnot(
    is(object, "DESeqAnalysis"),
    validObject(object),
    object.size(object) < limit
)
deseqMinimal <- object
use_data(deseqMinimal, overwrite = TRUE, compress = "xz")
