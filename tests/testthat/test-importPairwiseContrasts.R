context("importPairwiseContrasts")

test_that("importPairwiseContrasts", {
    file <- system.file("extdata/contrasts.csv", package = "DESeqAnalysis")
    object <- importPairwiseContrasts(file)
    expected <- list(
        dox_induction_control = c(
            group = "group",
            numerator = "B",
            denominator = "A"
        ),
        effect_of_wt_protein_induction_vs_ev_line = c(
            group = "group",
            numerator = "C",
            denominator = "B"

        )
    )
    expect_identical(object, expected)
})
