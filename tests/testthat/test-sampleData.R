test_that("DESeqAnalysis", {
    object <- objs[["deseq"]]
    x <- sampleData(object)
    expect_s4_class(x, "DFrame")
})
