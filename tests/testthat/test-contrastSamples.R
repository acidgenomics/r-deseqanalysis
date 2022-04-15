context("contrastSamples")

test_that("DESeqAnalysis : character", {
    expect_identical(
        object = contrastSamples(
            deseq,
            i = 1L,
            return = "character",
            quiet = FALSE
        ),
        expected = c(
            "sample07",
            "sample08",
            "sample09",
            "sample10",
            "sample11",
            "sample12",
            "sample01",
            "sample02",
            "sample03",
            "sample04",
            "sample05",
            "sample06"
        )
    )
    expect_identical(
        object = contrastSamples(
            deseq,
            i = 2L,
            return = "character",
            quiet = FALSE
        ),
        expected = c(
            "sample04",
            "sample05",
            "sample06",
            "sample10",
            "sample11",
            "sample12",
            "sample01",
            "sample02",
            "sample03",
            "sample07",
            "sample08",
            "sample09"
        )
    )
})

test_that("DESeqAnalysis : list", {
    expect_identical(
        object = contrastSamples(
            deseq,
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
                    "sample07",
                    "sample08",
                    "sample09",
                    "sample10",
                    "sample11",
                    "sample12"
                ),
                "denominator" = c(
                    "sample01",
                    "sample02",
                    "sample03",
                    "sample04",
                    "sample05",
                    "sample06"
                )
            )
        )
    )
    expect_identical(
        object = contrastSamples(
            deseq,
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
                    "sample04",
                    "sample05",
                    "sample06",
                    "sample10",
                    "sample11",
                    "sample12"
                ),
                "denominator" = c(
                    "sample01",
                    "sample02",
                    "sample03",
                    "sample07",
                    "sample08",
                    "sample09"
                )
            )
        )
    )
})
