context("alphaSummary")

test_that("Default, no contrast specified.", {
    object <- alphaSummary(dds)
    expect_is(object, "matrix")
    expect_type(object, "integer")
    expect_equal(
        object = object,
        expected = matrix(
            ## nolint start
            data = c(
                 81,  64,  47,  30,   6,
                 75,  60,  39,  23,   4,
                  2,   2,   2,   2,   2,
                  0,   0,  96, 173,   0
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

test_that("Contrast vector", {
    expect_identical(
        object = alphaSummary(
            object = dds,
            contrast = c("condition", "B", "A")
        ),
        expected = alphaSummary(dds)
    )
})

test_that("Contrast name", {
    expect_identical(
        object = alphaSummary(
            object = dds,
            name = "condition_B_vs_A"
        ),
        expected = alphaSummary(dds)
    )
})

test_that("Contrast or name", {
    expect_error(
        alphaSummary(dds, contrast = "aaa", name = "bbb"),
        "Specify either `contrast` or `name`."
    )
})
