## FIXME Add code coverage for `extra = TRUE` for very minimal DESeqDataSet.

test_that("results argument", {
    object <- objs[["deseq"]]
    ## Numeric scalar.
    x <- results(object, i = 1L)
    expect_s4_class(x, "DESeqResults")
    ## Character string.
    x <- results(object, i = resultsNames(object)[[1L]])
    expect_s4_class(x, "DESeqResults")
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
