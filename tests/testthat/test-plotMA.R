context("plotMA")

args <- list(object = deseq, results = 1L)

test_that("DESeqAnalysis", {
    x <- do.call(plotMA, args)
    expect_is(x, "ggplot")

    # Check geom classes.
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

    # Check plot labels.
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
        args = c(
            args,
            list(
                direction = "up",
                sigPointColor = "red"
            )
        )
    )
    expect_is(x, "ggplot")
    x <- do.call(
        what = plotMA,
        args = c(
            args,
            list(
                direction = "down",
                sigPointColor = "green"
            )
        )
    )
    expect_is(x, "ggplot")
})

test_that("Label the top genes", {
    x <- do.call(
        what = plotMA,
        args = c(args, list(ntop = 10L))
    )
    expect_is(x, "ggplot")
})

test_that("Label specific genes", {
    x <- do.call(
        what = plotMA,
        args = c(
            args,
            list(genes = geneNames)
        )
    )
    expect_is(x, "ggplot")
})

test_that("DataFrame return", {
    x <- do.call(
        what = plotMA,
        args = c(
            args,
            list(return = "DataFrame")
        )
    )
    expect_s4_class(x, "DataFrame")
    expect_true("isDE" %in% colnames(x))
})
