context("alphaSummary")

test_that("Default, no contrast specified.", {
    object <- alphaSummary(dds)
    expect_is(object, "matrix")
    expect_type(object, "integer")
    expect_equal(
        object = object,
        expected = matrix(
            # nolint start
            data = c(
                84, 69,  42,  23, 6,
                87, 68,  31,  16, 3,
                 2,  2,   2,   2, 2,
                 0, 77, 125, 115, 0
            ),
            # nolint end
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
