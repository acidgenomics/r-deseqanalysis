test_that("DESeqAnalysis", {
    object <- objs[["deseq"]]
    x <- results(object, i = 1L)
    expect_s4_class(x, "DESeqResults")
    x <- results(object, i = resultsNames(object)[[1L]])
    expect_s4_class(x, "DESeqResults")
    x <- results(object, i = 1L, extra = TRUE)
    expect_identical(
        object = colnames(x),
        expected = c(
            "baseMean",
            "log2FoldChange",
            "lfcSE",
            "stat",
            "pvalue",
            "padj",
            "broadClass",
            "description",
            "geneBiotype",
            "geneId",
            "geneIdNoVersion",
            "geneIdVersion",
            "geneName",
            "sample1",
            "sample2",
            "sample3",
            "sample4",
            "sample5",
            "sample6",
            "sample7",
            "sample8",
            "sample9",
            "sample10",
            "sample11",
            "sample12"
        )
    )
    x <- results(object, i = 1L, extra = FALSE)
    expect_identical(
        object = colnames(x),
        expected = c(
            "baseMean",
            "log2FoldChange",
            "lfcSE",
            "stat",
            "pvalue",
            "padj"
        )
    )
})

test_that("DESeqAnalysis without rowData", {
    object <- objs[["deseqMinimal"]]
    x <- results(object, i = 1L, extra = TRUE)
    expect_identical(
        object = colnames(x),
        expected = c(
            "baseMean",
            "log2FoldChange",
            "lfcSE",
            "stat",
            "pvalue",
            "padj",
            "sample1",
            "sample2",
            "sample3",
            "sample4"
        )
    )
})

test_that("Match failure", {
    object <- objs[["deseq"]]
    expect_error(
        object = results(object),
        regexp = "missing"
    )
    expect_error(
        object = results(object, i = "XXX"),
        regexp = "XXX"
    )
})
