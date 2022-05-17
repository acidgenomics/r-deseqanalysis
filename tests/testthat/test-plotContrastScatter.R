test_that("DESeqAnalysis", {
    object <- objs[["deseq"]]
    p <- plotContrastScatter(object, i = 1L)
    expect_s3_class(p, "ggplot")
})
