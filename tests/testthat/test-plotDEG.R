context("plotDEGHeatmap")

test_that("DESeqAnalysis", {
    expect_is(
        object = plotDEGHeatmap(deseq, results = 1L),
        class = "pheatmap"
    )
})



context("plotDEGPCA")

test_that("DESeqAnalysis", {
    expect_is(
        object = plotDEGPCA(deseq, results = 1L),
        class = "ggplot"
    )
})
