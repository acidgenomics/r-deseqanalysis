test_that("DESeqAnalysis", {
    object <- objs[["deseq"]]
    p <- plotLFC(object)
    expect_is(p, "ggplot")
})
