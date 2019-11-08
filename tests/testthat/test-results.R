context("results")

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

test_that("lfcShrink results requested, but not slotted", {
    object <- deseq
    object@lfcShrink <- list()
    expect_true(validObject(object))
    x <- results(object, i = 1L, lfcShrink = FALSE)
    expect_s4_class(x, "DESeqResults")
    expect_error(
        object = results(object, i = 1L, lfcShrink = TRUE),
        regexp = "lfcShrink"
    )
})
