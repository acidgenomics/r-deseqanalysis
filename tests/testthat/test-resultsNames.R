context("resultsNames")

test_that("DESeqAnalysis", {
    object <- objs[["deseq"]]
    expect_identical(
        object = resultsNames(object),
        expected = c(
            "condition_B_vs_A",
            "treatment_D_vs_C"
        )
    )
    resultsNames(object) <- c("XXX", "YYY")
    expect_identical(
        object = resultsNames(object),
        expected = c("XXX", "YYY")
    )
})
