context("plotLFC")

test_that("DESeqAnalysis", {
    p <- plotLFC(deseq)
    expect_is(p, "ggplot")
})
