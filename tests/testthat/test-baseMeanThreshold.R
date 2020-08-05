context("baseMeanThreshold")

test_that("DESeqAnalysis", {
    value <- 0L
    baseMeanThreshold(deseq) <- NULL
    expect_identical(baseMeanThreshold(deseq), value)
    value <- 1L
    baseMeanThreshold(deseq) <- value
    expect_identical(baseMeanThreshold(deseq), value)
})
