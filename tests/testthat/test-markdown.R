context("markdown")

test_that("DESeqAnalysis", {
    x <- capture.output(markdown(deseq))
    expect_match(x[[1L]], "## Contrast names")
})
