context("alphaSummary")

test_that("Default, no contrast specified.", {
    object <- alphaSummary(
        object = dds,
        contrast = c("condition", "B", "A")
    )
    expect_is(object, "matrix")
    expect_type(object, "integer")
    expect_equal(
        object = object,
        expected = matrix(
            ## nolint start
            data = c(
                 52,  42,  22,  18,   2,
                 43,  30,  18,   9,   0,
                  0,   0,   0,   0,   0,
                 49, 155,  68, 271,   0
            ),
            ## nolint end
            nrow = 4L,
            ncol = 5L,
            byrow = TRUE,
            dimnames = list(
                c(
                    "LFC > 0 (up)",
                    "LFC < 0 (down)",
                    "outliers [1]",
                    "low counts [2]"
                ),
                c(1e-01, 0.5e-01, 1e-02, 1e-03, 1e-06)
            )
        )
    )
})

test_that("Contrast vector or name", {
    expect_identical(
        object = alphaSummary(
            object = dds,
            name = "condition_B_vs_A"
        ),
        expected = alphaSummary(
            object = dds,
            contrast = c("condition", "B", "A")
        )
    )
    expect_error(
        alphaSummary(dds, contrast = "aaa", name = "bbb"),
        "Specify either 'contrast' or 'name'."
    )
})
