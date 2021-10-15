context("export : DESeqAnalysis")

testdir <- file.path(tempdir(), "example")

test_that("Deprecated : 'dir' instead of 'con'", {
    unlink(testdir, recursive = TRUE)
    object <- deseq
    out <- export(
        object = object,
        dir = testdir,
        compress = TRUE
    )
    prefix <- realpath(file.path(testdir, "object"))
    resTblPrefix <- file.path(prefix, "resultsTables")
    resMatPrefix <- file.path(prefix, "resultsMatrices")

    ## FIXME This is outputting "counts", "mu", "H", and "cooks" in assays.

    expect_identical(
        object = out,
        expected = list(
            "data" = list(
                "assays" = list(
                    "counts" = file.path(
                        prefix,
                        "data",
                        "assays",
                        "counts.csv.gz"
                    ),
                    "normalized" = file.path(
                        prefix,
                        "data",
                        "assays",
                        "normalized.csv.gz"
                    ),
                    "fpkm" = file.path(
                        prefix,
                        "data",
                        "assays",
                        "fpkm.csv.gz"
                    )
                ),
                "colData" = file.path(prefix, "data", "colData.csv.gz"),
                "rowData" = file.path(prefix, "data", "rowData.csv.gz")
            ),
            "transform" = list(
                "assays" = list(
                    "assay" = file.path(
                        prefix,
                        "transform",
                        "assays",
                        "assay.csv.gz"
                    )
                ),
                "colData" = file.path(prefix, "transform", "colData.csv.gz"),
                "rowData" = file.path(prefix, "transform", "rowData.csv.gz")
            ),
            "resultsTables" = list(
                "condition_B_vs_A" = c(
                    "all" = file.path(
                        resTblPrefix, "condition_B_vs_A", "all.csv.gz"
                    ),
                    "up" = file.path(
                        resTblPrefix, "condition_B_vs_A", "up.csv.gz"
                    ),
                    "down" = file.path(
                        resTblPrefix, "condition_B_vs_A", "down.csv.gz"
                    ),
                    "both" = file.path(
                        resTblPrefix, "condition_B_vs_A", "both.csv.gz"
                    )
                ),
                "treatment_D_vs_C" = c(
                    "all" = file.path(
                        resTblPrefix, "treatment_D_vs_C", "all.csv.gz"
                    )
                )
            ),
            "resultsMatrices" = c(
                "log2FoldChange" = file.path(
                    resMatPrefix, "log2FoldChange.csv.gz"
                ),
                "stat" = file.path(
                    resMatPrefix, "stat.csv.gz"
                ),
                "padj" = file.path(
                    resMatPrefix, "padj.csv.gz"
                )
            )
        )
    )
    unlink(testdir, recursive = TRUE)
})
