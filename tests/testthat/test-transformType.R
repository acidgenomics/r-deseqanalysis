test_that("DESeqAnalysis", {
    object <- objs[["deseq"]]
    expect_identical(
        object = transformType(object),
        expected = "varianceStabilizingTransformation"
    )
})
