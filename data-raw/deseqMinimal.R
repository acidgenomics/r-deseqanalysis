## nolint start
suppressPackageStartupMessages({
    library(devtools)
    library(usethis)
    library(DESeq2)
})
## nolint end
load_all(helpers = FALSE)
dds <- makeExampleDESeqDataSet(n = 50L, m = 4L)
dds <- DESeq(dds, fitType = "local")
dt <- varianceStabilizingTransformation(dds, fitType = "local")
res <- results(dds)
object <- DESeqAnalysis(
    data = dds,
    transform = dt,
    results = res,
    lfcShrink = NULL
)
limit <- structure(1e6L, class = "object_size") # nolint
stopifnot(
    is(object, "DESeqAnalysis"),
    validObject(object),
    object.size(object) < limit
)
deseqMinimal <- object
use_data(deseqMinimal, overwrite = TRUE, compress = "xz")
