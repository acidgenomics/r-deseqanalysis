test_that("DESeqAnalysis", {
    object <- objs[["deseq"]]
    p <- plotDEGUpset(object)
    expect_s3_class(p, "patchwork")
})
