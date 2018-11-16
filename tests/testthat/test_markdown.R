context("Markdown")

data(deseq, envir = environment())
res_small <- as(deseq, "DESeqResults")
res_tables <- DESeqResultsTables(res_small)



# topTables ====================================================================
with_parameters_test_that(
    "topTables", {
        x <- capture.output(topTables(object))
        expect_true(grepl("padj", x[[3L]]))
    },
    object = list(
        DESeqAnalysis = deseq,
        DESeqResults = res_small,
        DESeqResultsTables = res_tables
    )
)
