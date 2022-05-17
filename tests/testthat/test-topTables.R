## FIXME Need to test support for more minimal DESeqDataSet / DESeqResults

test_that("DESeqAnalysis", {
    for (object in objs[c("deseq", "deseqMinimal")]) {
        x <- capture.output(topTables(object, i = 1L))
        expect_true(any(grepl("\\|padj", x)))
    }
})

test_that("DESeqResults", {
    deseq <- objs[["deseq"]]
    res <- results(deseq, i = 1L)
    dds <- as(deseq, "DESeqDataSet")
    ## Minimal return.
    x <- capture.output(topTables(res))
    expect_false(any(grepl("\\|geneName", x)))
    ## Extra mode, using DESeqDataSet.
    x <- capture.output(topTables(object = res, DESeqDataSet = dds))
    expect_true(any(grepl("\\|geneName", x)))
})
