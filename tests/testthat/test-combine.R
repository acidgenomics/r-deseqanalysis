context("combine")

test_that("DESeqAnalysis", {
    x <- deseq
    y <- deseq
    namesX <- paste0("x_", resultsNames(x))
    namesY <- paste0("y_", resultsNames(y))
    resultsNames(x) <- namesX
    resultsNames(y) <- namesY
    object <- combine(x, y)
    expect_s4_class(object, "DESeqAnalysis")
    expect_identical(
        object = resultsNames(object),
        expected = c(namesX, namesY)
    )
})
