context("updateObject")

test_that("DESeqAnalysis", {
    x <- updateObject(deseq)
    expect_s4_class(x, "DESeqAnalysis")
})
