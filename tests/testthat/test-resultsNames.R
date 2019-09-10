context("resultsNames")

test_that("DESeqAnalysis", {
    object <- deseq
    expect_identical(
        resultsNames(object),
        "condition_B_vs_A"
    )
    resultsNames(object) <- "XXX"
    expect_identical(
        resultsNames(object),
        "XXX"
    )
})
