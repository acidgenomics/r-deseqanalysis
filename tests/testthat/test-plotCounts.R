context("plotCounts")

test_that("DESeqAnalysis", {
    ## Get genes from working example.
    res <- results(deseq, results = 1L)
    genes <- head(rownames(res))

    p <- plotCounts(deseq, genes = genes, style = "facet")
    expect_s3_class(p, "ggplot")

    p <- plotCounts(deseq, genes = genes, style = "wide")
    expect_s3_class(p, "ggplot")
})
