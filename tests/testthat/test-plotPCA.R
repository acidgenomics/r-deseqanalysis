context("plotPCA")

test_that("DESeqAnalysis", {
    p <- plotPCA(deseq)
    expect_s3_class(p, "ggplot")
})

test_that("DESeqTransform", {
    dt <- as(deseq, "DESeqTransform")
    p <- plotPCA(dt)
    expect_s3_class(p, "ggplot")
})
