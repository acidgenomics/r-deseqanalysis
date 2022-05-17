test_that("DESeqAnalysis", {
    object <- objs[["deseq"]]
    p <- plotDEGStackedBar(object)
    expect_s3_class(p, "ggplot")
})
