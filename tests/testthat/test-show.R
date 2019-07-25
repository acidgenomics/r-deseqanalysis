context("show")

test_that("DESeqAnalysis", {
    x <- capture.output(show(deseq))
    expect_match(x[[1L]], "DESeqAnalysis")
})
