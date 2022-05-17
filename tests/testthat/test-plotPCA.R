test_that("DESeqAnalysis", {
    object <- objs[["deseq"]]
    p <- plotPCA(object)
    expect_s3_class(p, "ggplot")
})

test_that("DESeqTransform", {
    object <- as.DESeqTransform(objs[["deseq"]])
    p <- plotPCA(object)
    expect_s3_class(p, "ggplot")
})
