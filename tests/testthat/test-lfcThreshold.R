test_that("DESeqAnalysis", {
    object <- objs[["deseq"]]
    value <- metadata(object@results[[1L]])[["lfcThreshold"]]
    expect_is(value, "integer")
    lfcThreshold(object) <- NULL
    expect_identical(lfcThreshold(object), value)
    value <- 1L
    lfcThreshold(object) <- value
    expect_identical(lfcThreshold(object), value)
})
