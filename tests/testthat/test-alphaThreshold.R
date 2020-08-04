context("alphaThreshold")

test_that("DESeqAnalysis", {
    value <- metadata(deseq@results[[1L]])[["alpha"]]
    expect_is(value, "numeric")
    alphaThreshold(deseq) <- NULL
    expect_identical(alphaThreshold(deseq), value)
    value <- 0.05
    alphaThreshold(deseq) <- value
    expect_identical(alphaThreshold(deseq), value)
})
