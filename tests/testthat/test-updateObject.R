test_that("DESeqAnalysis", {
    object <- objs[["deseq"]]
    object <- updateObject(object)
    expect_s4_class(object, "DESeqAnalysis")
})
