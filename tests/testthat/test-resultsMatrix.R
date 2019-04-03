context("resultsMatrix")

test_that("resultsMatrix", {
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
