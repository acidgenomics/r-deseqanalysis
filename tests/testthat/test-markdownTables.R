test_that("DESeqAnalysis with extra rowData", {
    object <- objs[["deseq"]]
    out <- capture.output(markdownTables(object, i = 1L))
    x <- out[[5L]]
    x <- strsplit(x = x, split = "|", fixed = TRUE)[[1L]]
    x <- gsub(pattern = " ", replacement = "", x = x)
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
    x <- out[[7L]]
    x <- strsplit(x = x, split = "|", fixed = TRUE)[[1L]]
    x <- gsub(pattern = "^[[:space:]]+", replacement = "", x = x)
    x <- gsub(pattern = "[[:space:]]+$", replacement = "", x = x)
    expect_identical(
        object = x,
        expected = c(
            "",
            "gene65",
            "61",
            "2.40",
            "2.35e-04",
            "coding",
            "ARX",
            "aristaless related homeobox"
        )
    )
})

test_that("DESeqAnalysis with minimal rowData", {
    object <- objs[["deseq"]]
    rowData(object@data) <- NULL
    x <- capture.output(markdownTables(object, i = 1L))
    x <- x[[5L]]
    x <- strsplit(x = x, split = "|", fixed = TRUE)[[1L]]
    x <- gsub(pattern = " ", replacement = "", x = x)
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
    x <- capture.output(markdownTables(object, i = 1L))
    expect_identical(x, character())
})
