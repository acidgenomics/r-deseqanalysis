context("Markdown")

data(deseq, envir = environment())



# topTables ====================================================================
test_that("topTables", {
    object <- deseq
    x <- capture.output(topTables(object))
    expect_true(grepl("padj", x[[3L]]))
})
