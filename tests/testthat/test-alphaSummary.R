context("alphaSummary")

object <- objs[["deseq"]]

## NOTE The expected values can change when we resave the example object.
test_that("Default, no contrast specified.", {
    object <- alphaSummary(
        object = object,
        contrast = c("condition", "B", "A")
    )
    expect_type(object, "integer")
    expect_true(is.matrix(object))
    expect_equal(
        object = object,
        expected = matrix(
            ## nolint start
            data = c(
                39, 34, 17, 3, 0,
                31, 23, 13, 6, 1,
                0, 0, 0, 0, 0,
                97, 49, 87, 0, 0
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
            object = object,
            name = "condition_B_vs_A"
        ),
        expected = alphaSummary(
            object = object,
            contrast = c("condition", "B", "A")
        )
    )
    expect_error(
        object = alphaSummary(deseq, contrast = "aaa", name = "bbb"),
        regexp = "either"
    )
})
