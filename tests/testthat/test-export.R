test_that("New 'con' BiocIO approach, instead of deprecated 'dir'", {
    testdir <- tempdir2()
    object <- objs[["deseq"]]
    out <- export(
        object = object,
        con = testdir,
        compress = TRUE
    )
    prefix <- realpath(testdir)
    resTblPrefix <- file.path(prefix, "resultsTables")
    resMatPrefix <- file.path(prefix, "resultsMatrices")
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
    counts <- import(out[["data"]][["assays"]][["counts"]])
    expect_identical(
        object = colnames(counts)[[1L]],
        expected = "sample1"
    )
    expect_identical(
        object = rownames(counts)[[1L]],
        expected = "gene1"
    )
    res <- import(out[["resultsTables"]][[1L]][["all"]])
    expect_identical(
        object = rownames(res)[[1L]],
        expected = "gene1"
    )
    expect_identical(
        object = colnames(res),
        expected = c(
            "baseMean",
            "log2FoldChange",
            "lfcSE",
            "pvalue",
            "padj",
            "broadClass",
            "description",
            "geneBiotype",
            "geneId",
            "geneIdNoVersion",
            "geneIdVersion",
            "geneName",
            "sample1",
            "sample2",
            "sample3",
            "sample4",
            "sample5",
            "sample6",
            "sample7",
            "sample8",
            "sample9",
            "sample10",
            "sample11",
            "sample12"
        )
    )
    unlink2(testdir)
})

test_that("Deprecated : 'dir' argument, no 'name'", {
    testdir <- tempdir2()
    object <- objs[["deseq"]]
    out <- export(
        object = object,
        dir = testdir,
        compress = TRUE
    )
    prefix <- realpath(file.path(testdir, "object"))
    resTblPrefix <- file.path(prefix, "resultsTables")
    resMatPrefix <- file.path(prefix, "resultsMatrices")
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
    unlink2(testdir)
})
