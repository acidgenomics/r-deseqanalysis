context("DESeqAnalysisList")

test_that("list", {
    x <- DESeqAnalysisList(list("object1" = deseq, "object2" = deseq))
    expect_s4_class(x, "DESeqAnalysisList")
})
