test_that("DESeqAnalysis : character", {
    object <- objs[["deseq"]]
    expect_identical(
        object = contrastSamples(
            object = object,
            i = 1L,
            return = "character",
            quiet = FALSE
        ),
        expected = c(
            "sample7",
            "sample8",
            "sample9",
            "sample10",
            "sample11",
            "sample12",
            "sample1",
            "sample2",
            "sample3",
            "sample4",
            "sample5",
            "sample6"
        )
    )
    expect_identical(
        object = contrastSamples(
            object = object,
            i = 2L,
            return = "character",
            quiet = FALSE
        ),
        expected = c(
            "sample4",
            "sample5",
            "sample6",
            "sample10",
            "sample11",
            "sample12",
            "sample1",
            "sample2",
            "sample3",
            "sample7",
            "sample8",
            "sample9"
        )
    )
})

test_that("DESeqAnalysis : list", {
    object <- objs[["deseq"]]
    expect_identical(
        object = contrastSamples(
            object = object,
            i = 1L,
            return = "list",
            quiet = FALSE
        ),
        expected = list(
            "contrast" = c(
                "factor" = "condition",
                "numerator" = "B",
                "denominator" = "A"
            ),
            "samples" = list(
                "numerator" = c(
                    "sample7",
                    "sample8",
                    "sample9",
                    "sample10",
                    "sample11",
                    "sample12"
                ),
                "denominator" = c(
                    "sample1",
                    "sample2",
                    "sample3",
                    "sample4",
                    "sample5",
                    "sample6"
                )
            )
        )
    )
    expect_identical(
        object = contrastSamples(
            object = object,
            i = 2L,
            return = "list",
            quiet = FALSE
        ),
        expected = list(
            "contrast" = c(
                "factor" = "treatment",
                "numerator" = "D",
                "denominator" = "C"
            ),
            "samples" = list(
                "numerator" = c(
                    "sample4",
                    "sample5",
                    "sample6",
                    "sample10",
                    "sample11",
                    "sample12"
                ),
                "denominator" = c(
                    "sample1",
                    "sample2",
                    "sample3",
                    "sample7",
                    "sample8",
                    "sample9"
                )
            )
        )
    )
})
