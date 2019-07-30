context("results")

test_that("results argument", {
    ## Numeric scalar.
    x <- results(deseq, results = 1L)
    expect_s4_class(x, "DESeqResults")

    ## Character string.
    x <- results(deseq, results = resultsNames(deseq)[[1L]])
    expect_s4_class(x, "DESeqResults")
})

test_that("Match failure", {
    expect_error(
        object = results(deseq),
        regexp = "missing"
    )
    expect_error(
        object = results(deseq, results = "XXX"),
        regexp = "XXX"
    )
})

test_that("lfcShrink results requested, but not slotted", {
    object <- deseq
    object@lfcShrink <- list()
    expect_true(validObject(object))
    x <- results(object, results = 1L, lfcShrink = FALSE)
    expect_s4_class(x, "DESeqResults")
    expect_error(
        object = results(object, results = 1L, lfcShrink = TRUE),
        regexp = "lfcShrink"
    )
})
