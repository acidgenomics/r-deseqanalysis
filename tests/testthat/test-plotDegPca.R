test_that("DESeqAnalysis", {
    object <- objs[["deseq"]]
    p <- plotDegPca(object, i = 1L)
    expect_s3_class(p, "ggplot")
})
