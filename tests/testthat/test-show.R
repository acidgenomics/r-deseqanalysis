test_that("DESeqAnalysis", {
    object <- objs[["deseq"]]
    x <- capture.output(show(object))
    expect_match(x[[1L]], "DESeqAnalysis")
})
