test_that("DESeqAnalysis", {
    object <- objs[["deseq"]]
    res <- results(object, i = 1L)
    genes <- head(rownames(res))
    p <- plotCounts(object, genes = genes, style = "facet")
    expect_s3_class(p, "ggplot")
    p <- plotCounts(object, genes = genes, style = "wide")
    expect_s3_class(p, "ggplot")
})
