context("transformType")

test_that("DESeqAnalysis", {
    x <- transformType(deseq)
    expect_identical(x, "varianceStabilizingTransformation")
})
