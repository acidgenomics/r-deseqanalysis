test_that("DESeqAnalysis", {
    object <- objs[["deseq"]]
    value <- 0L
    baseMeanThreshold(object) <- NULL
    expect_identical(baseMeanThreshold(object), value)
    value <- 1L
    baseMeanThreshold(object) <- value
    expect_identical(baseMeanThreshold(object), value)
})
