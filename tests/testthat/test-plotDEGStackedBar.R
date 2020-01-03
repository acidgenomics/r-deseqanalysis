context("plotDEGStackedBar")

test_that("DESeqAnalysis", {
    p <- plotDEGStackedBar(deseq)
    expect_s3_class(p, "ggplot")
})
