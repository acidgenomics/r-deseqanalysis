test_that("DESeqAnalysis", {
    object <- objs[["deseq"]]
    p <- plotDEGPCA(object, i = 1L)
    expect_s3_class(p, "ggplot")
})
