context("topTables")

test_that("DESeqAnalysis", {
    x <- capture.output(topTables(deseq, i = 1L))
    expect_true(any(grepl("\\|padj", x)))
})

test_that("DESeqResults", {
    res <- results(deseq, i = 1L)
    dds <- as(deseq, "DESeqDataSet")
    ## Minimal return.
    x <- capture.output(topTables(res))
    expect_false(any(grepl("\\|geneName", x)))
    ## Extra mode, using DESeqDataSet.
    x <- capture.output(topTables(res, DESeqDataSet = dds))
    expect_true(any(grepl("\\|geneName", x)))
})
