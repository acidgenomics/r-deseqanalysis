test_that("DESeqAnalysis", {
    object <- objs[["deseq"]]
    object <- transformType(object)
    expect_identical(object, "varianceStabilizingTransformation")
})
