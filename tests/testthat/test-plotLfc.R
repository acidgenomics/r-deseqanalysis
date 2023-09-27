test_that("DESeqAnalysis", {
    object <- objs[["deseq"]]
    p <- plotLfc(object)
    expect_s3_class(p, "ggplot")
})
