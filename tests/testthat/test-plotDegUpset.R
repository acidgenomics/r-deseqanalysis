test_that("DESeqAnalysis", {
    object <- objs[["deseq"]]
    p <- plotDegUpset(object)
    expect_s3_class(p, "patchwork")
})
