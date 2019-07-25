context("plotDEGPCA")

test_that("DESeqAnalysis", {
    expect_is(
        object = plotDEGPCA(deseq, results = 1L),
        class = "ggplot"
    )
})
