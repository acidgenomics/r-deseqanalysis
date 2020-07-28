context("resultsMatrix")

test_that("DESeqAnalysis", {
    object <- deseq
    mat <- resultsMatrix(object, value = "log2FoldChange")
    expect_identical(
        object = dimnames(mat),
        expected = list(
            rownames(slot(deseq, "data")),
            resultsNames(object)
        )
    )
    expect_identical(
        object = mat[, 1L, drop = TRUE],
        expected = deseq@lfcShrink[[1L]][["log2FoldChange"]]
    )
})

test_that("Unshrunken values", {
    object <- deseq
    object@lfcShrink <- list()
    x <- resultsMatrix(object, value = "log2FoldChange")
    expect_is(x, "matrix")
})

test_that("value argument", {
    for (value in eval(formals(`resultsMatrix,DESeqAnalysis`)[["value"]])) {
        x <- resultsMatrix(deseq, value = value)
        expect_is(x, "matrix")
    }
})

test_that("DESeqAnalysisList", {
    object <- DESeqAnalysisList(deseq)
    x <- resultsMatrix(object)
    expect_is(x, "matrix")
    expect_identical(
        object = colnames(x),
        expected = c(
            "deseq_condition_B_vs_A",
            "deseq_treatment_D_vs_C"
        )
    )
})
