context("DESeqAnalysisList")

test_that("DESeqAnalysisList", {
    x <- DESeqAnalysisList(deseq)
    expect_s4_class(x, "DESeqAnalysisList")
})
