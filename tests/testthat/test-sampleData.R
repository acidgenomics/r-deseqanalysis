context("sampleData")

test_that("DESeqAnalysis", {
    x <- sampleData(deseq)
    expect_s4_class(x, "DataFrame")
})
