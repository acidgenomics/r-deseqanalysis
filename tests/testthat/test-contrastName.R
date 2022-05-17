test_that("DESeqResults", {
    object <- results(objs[["deseq"]], i = 1L)
    expect_identical(
        object = contrastName(object),
        expected = "condition_B_vs_A"
    )
    contrastName(object) <- "XXX"
    expect_identical(
        object = contrastName(object),
        expected = "XXX"
    )
})
