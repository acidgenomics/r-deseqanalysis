test_that("DESeqAnalysis", {
    object <- objs[["deseq"]]
    value <- metadata(object@results[[1L]])[["lfcThreshold"]]
    expect_type(value, "integer")
    lfcThreshold(object) <- NULL
    expect_identical(lfcThreshold(object), value)
    value <- 1L
    lfcThreshold(object) <- value
    expect_identical(lfcThreshold(object), value)
})
