context("contrastSamples")

## Need to update example object to include multiple contrasts.

test_that("DESeqAnalysis", {
    x <- contrastSamples(deseq, i = 1L)
    expect_is(x, "character")
})
