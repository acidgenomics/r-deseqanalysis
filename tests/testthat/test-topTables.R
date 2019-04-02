context("topTables")

test_that("DESeqAnalysis", {
    x <- capture.output(topTables(deseq, results = 1L))
    expect_true(grepl("padj", x[[3L]]))
})
