context("Markdown")

data(deseq, envir = environment())



# topTables ====================================================================
test_that("topTables", {
    x <- capture.output(topTables(deseq, results = 1L))
    expect_true(grepl("padj", x[[3L]]))
})
