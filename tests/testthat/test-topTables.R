test_that("DESeqAnalysis", {
    object <- objs[["deseq"]]
    x <- capture.output(topTables(object, i = 1L))
    x <- x[[5L]]
    x <- gsub(pattern = " ", replacement = "", x = x)
    x <- strsplit(x = x, split = "|", fixed = TRUE)[[1L]]
    expect_identical(
        object = x,
        expected = c(
            "",
            "",
            "baseMean",
            "lfc",
            "padj",
            "broadClass",
            "geneName",
            "description"
        )
    )
    object <- objs[["deseq"]]
    rowData(object@data) <- NULL
    x <- capture.output(topTables(object, i = 1L))
    x <- x[[5L]]
    x <- gsub(pattern = " ", replacement = "", x = x)
    x <- strsplit(x = x, split = "|", fixed = TRUE)[[1L]]
    expect_identical(
        object = x,
        expected = c(
            "",
            "",
            "baseMean",
            "lfc",
            "padj"
        )
    )
    object <- objs[["deseqMinimal"]]
    x <- capture.output(topTables(object, i = 1L))
    expect_identical(x, character())
})
