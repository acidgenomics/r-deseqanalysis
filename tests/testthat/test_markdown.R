context("Markdown")

data(deseq, envir = environment())

res <- as(deseq, "DESeqResults")
resTbl <- DESeqResultsTables(res)



# topTables ====================================================================
with_parameters_test_that(
    "topTables", {
        x <- capture.output(topTables(object))
        expect_true(grepl("padj", x[[3L]]))
    },
    object = list(
        DESeqAnalysis = deseq,
        DESeqResults = res,
        DESeqResultsTables = resTbl
    )
)
