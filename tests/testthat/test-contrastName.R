context("contrastName")

test_that("DESeqResults", {
    object <- results(deseq, results = 1L)
    expect_identical(
        contrastName(object),
        "condition_B_vs_A"
    )
    contrastName(object) <- "XXX"
    expect_identical(
        contrastName(object),
        "XXX"
    )
})
