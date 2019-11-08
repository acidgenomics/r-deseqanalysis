context("plotDEGPCA")

test_that("DESeqAnalysis", {
    expect_is(
        object = plotDEGPCA(deseq, i = 1L),
        class = "ggplot"
    )
})
