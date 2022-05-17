context("resultsMatrix")

test_that("DESeqAnalysis", {
    object <- objs[["deseq"]]
    mat <- resultsMatrix(object, value = "log2FoldChange")
    expect_identical(
        object = dimnames(mat),
        expected = list(
            rownames(slot(object, "data")),
            resultsNames(object)
        )
    )
    expect_identical(
        object = mat[, 1L, drop = TRUE],
        expected = object@lfcShrink[[1L]][["log2FoldChange"]]
    )
})

test_that("Unshrunken values", {
    object <- objs[["deseq"]]
    object@lfcShrink <- list()
    x <- resultsMatrix(object, value = "log2FoldChange")
    expect_is(x, "matrix")
})

test_that("value argument", {
    object <- objs[["deseq"]]
    values <- eval(formals(`resultsMatrix,DESeqAnalysis`)[["value"]])
    for (value in values) {
        x <- resultsMatrix(object, value = value)
        expect_is(x, "matrix")
    }
})

test_that("DESeqAnalysisList", {
    object <- DESeqAnalysisList(list(
        "object1" = objs[["deseq"]],
        "object2" = objs[["deseq"]]
    ))
    x <- resultsMatrix(object)
    expect_is(x, "matrix")
    expect_identical(
        object = colnames(x),
        expected = c(
            "object1_condition_B_vs_A",
            "object1_treatment_D_vs_C",
            "object2_condition_B_vs_A",
            "object2_treatment_D_vs_C"
        )
    )
})
