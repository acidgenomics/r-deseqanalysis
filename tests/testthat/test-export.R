context("export")

test_that("DESeqAnalysis", {
    prefix <- realpath(file.path("XXX", "AAA"))
    resultsPrefix <- file.path(prefix, "resultsTables", "condition_B_vs_A")
    x <- export(
        object = deseq,
        name = "AAA",
        dir = "XXX",
        compress = TRUE,
        lfcShrink = TRUE
    )
    expect_identical(
        object = x,
        expected = list(
            data = list(
                assays = list(
                    counts = file.path(
                        prefix,
                        "data",
                        "assays",
                        "counts.csv.gz"
                    ),
                    normalized = file.path(
                        prefix,
                        "data",
                        "assays",
                        "normalized.csv.gz"
                    )
                ),
                colData = file.path(prefix, "data", "colData.csv.gz"),
                rowData = file.path(prefix, "data", "rowData.csv.gz")
            ),
            transform = list(
                assays = list(
                    assay = file.path(
                        prefix,
                        "transform",
                        "assays",
                        "assay.csv.gz"
                    )
                ),
                colData = file.path(prefix, "transform", "colData.csv.gz"),
                rowData = file.path(prefix, "transform", "rowData.csv.gz")
            ),
            resultsTables = list(
                condition_B_vs_A = c(
                    all = file.path(resultsPrefix, "all.csv.gz"),
                    up = file.path(resultsPrefix, "up.csv.gz"),
                    down = file.path(resultsPrefix, "down.csv.gz"),
                    both = file.path(resultsPrefix, "both.csv.gz")
                )
            )
        )
    )
    unlink("XXX", recursive = TRUE)
})
