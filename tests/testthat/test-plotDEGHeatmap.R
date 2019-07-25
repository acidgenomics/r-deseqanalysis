context("plotDEGHeatmap")

test_that("DESeqAnalysis", {
    expect_is(
        object = plotDEGHeatmap(deseq, results = 1L),
        class = "pheatmap"
    )
})
