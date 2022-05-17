test_that("DESeqAnalysis", {
    object <- objs[["deseq"]]
    value <- metadata(object@results[[1L]])[["alpha"]]
    expect_type(value, "double")
    alphaThreshold(object) <- NULL
    expect_identical(alphaThreshold(object), value)
    value <- 0.05
    alphaThreshold(object) <- value
    expect_identical(alphaThreshold(object), value)
})
