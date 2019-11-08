context("plotDEGHeatmap")

test_that("DESeqAnalysis", {
    expect_is(
        object = plotDEGHeatmap(deseq, i = 1L),
        class = "pheatmap"
    )
})
