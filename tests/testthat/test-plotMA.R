args <- list("object" = objs[["deseq"]], "i" = 1L)

test_that("DESeqAnalysis", {
    x <- do.call(plotMA, args)
    expect_s3_class(x, "ggplot")
    geomtype <- vapply(
        X = x[["layers"]],
        FUN = function(x) {
            class(x[["geom"]])[[1L]]
        },
        FUN.VALUE = character(1L)
    )
    expect_identical(
        object = geomtype,
        expected = c("GeomHline", "GeomPoint", "GeomLogticks")
    )
    expect_identical(
        object = x[["labels"]][["y"]],
        expected = "log2 fold change"
    )
    expect_identical(
        object = x[["labels"]][["x"]],
        expected = "mean expression across all samples"
    )
})

test_that("Directional support", {
    x <- do.call(
        what = plotMA,
        args = append(
            x = args,
            values = list("direction" = "up")
        )
    )
    expect_s3_class(x, "ggplot")
    x <- do.call(
        what = plotMA,
        args = append(
            x = args,
            values = list("direction" = "down")
        )
    )
    expect_s3_class(x, "ggplot")
})

test_that("Label the top genes", {
    x <- do.call(
        what = plotMA,
        args = append(
            x = args,
            values = list("ntop" = 10L)
        )
    )
    expect_s3_class(x, "ggplot")
})

test_that("Label specific genes", {
    x <- do.call(
        what = plotMA,
        args = append(
            x = args,
            values = list("genes" = objs[["geneNames"]])
        )
    )
    expect_s3_class(x, "ggplot")
})
