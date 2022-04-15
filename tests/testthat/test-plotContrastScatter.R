context("plotContrastScatter")

test_that("DESeqAnalysis", {
    p <- plotContrastScatter(deseq, i = 1L)
    expect_is(p, "ggplot")
})
