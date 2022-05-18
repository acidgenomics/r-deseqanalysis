test_that("DESeqAnalysis", {
    object <- objs[["deseq"]]
    p <- plotLFC(object)
    expect_s3_class(p, "ggplot")
})
