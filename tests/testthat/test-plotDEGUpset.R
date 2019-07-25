context("plotDEGUpset")

test_that("DESeqAnalysis", {
    p <- plotDEGUpset(deseq)
    expect_s3_class(p, "upset")
})
