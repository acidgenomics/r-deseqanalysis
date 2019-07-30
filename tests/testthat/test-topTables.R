context("topTables")

test_that("DESeqAnalysis", {
    x <- capture.output(topTables(deseq, results = 1L))
    expect_true(grepl("padj", x[[3L]]))
})

test_that("DESeqResults", {
    res <- results(deseq, results = 1L)
    dds <- as(deseq, "DESeqDataSet")

    ## Minimal return.
    x <- capture.output(topTables(res))
    expect_false(grepl("geneName", x[[3L]]))

    ## Extra mode, using DESeqDataSet.
    x <- capture.output(topTables(res, DESeqDataSet = dds))
    expect_true(grepl("geneName", x[[3L]]))
})
