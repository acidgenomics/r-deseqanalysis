context("lfcThreshold")

test_that("DESeqAnalysis", {
    value <- metadata(deseq@results[[1L]])[["lfcThreshold"]]
    expect_is(value, "integer")
    lfcThreshold(deseq) <- NULL
    expect_identical(lfcThreshold(deseq), value)
    value <- 1L
    lfcThreshold(deseq) <- value
    expect_identical(lfcThreshold(deseq), value)
})
