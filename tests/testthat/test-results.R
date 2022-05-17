## FIXME Work on `extra = TRUE` support for very minimal DESeqDataSet.

test_that("results argument", {
    ## Numeric scalar.
    x <- results(deseq, i = 1L)
    expect_s4_class(x, "DESeqResults")
    ## Character string.
    x <- results(deseq, i = resultsNames(deseq)[[1L]])
    expect_s4_class(x, "DESeqResults")
})

test_that("Match failure", {
    expect_error(
        object = results(deseq),
        regexp = "missing"
    )
    expect_error(
        object = results(deseq, i = "XXX"),
        regexp = "XXX"
    )
})
