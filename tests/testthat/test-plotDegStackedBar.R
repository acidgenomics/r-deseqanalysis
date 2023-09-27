test_that("DESeqAnalysis", {
    object <- objs[["deseq"]]
    p <- plotDegStackedBar(object)
    expect_s3_class(p, "ggplot")
})
