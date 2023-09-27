test_that("DESeqAnalysis", {
    object <- objs[["deseq"]]
    p <- plotDegHeatmap(object, i = 1L)
    expect_s3_class(p, "pheatmap")
})
