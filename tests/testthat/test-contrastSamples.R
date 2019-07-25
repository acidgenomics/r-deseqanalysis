context("contrastSamples")

## Need to update example object to include multiple contrasts.

test_that("DESeqAnalysis", {
    x <- contrastSamples(deseq, results = 1L)
    expect_is(x, "character")
})
