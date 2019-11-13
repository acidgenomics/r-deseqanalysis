context("plotVolcano")

args <- list(object = deseq, i = 1L)

test_that("plotVolcano", {
    x <- do.call(plotVolcano, args)
    expect_is(x, "ggplot")
})

test_that("Enable histograms", {
    x <- do.call(
        what = plotVolcano,
        args = c(
            args,
            list(histograms = TRUE)
        )
    )
    expect_is(x, "ggplot")
})

test_that("Directional support", {
    x <- do.call(
        what = plotVolcano,
        args = c(
            args,
            list(direction = "up")
        )
    )
    expect_is(x, "ggplot")
    x <- do.call(
        what = plotVolcano,
        args = c(
            args,
            list(direction = "down")
        )
    )
    expect_is(x, "ggplot")
})

test_that("Label the top genes", {
    x <- do.call(
        what = plotVolcano,
        args = c(
            args,
            list(ntop = 5L)
        )
    )
    expect_is(x, "ggplot")
})

test_that("Label specific genes", {
    x <- do.call(
        what = plotVolcano,
        args = c(
            args,
            list(genes = geneNames)
        )
    )
    expect_is(x, "ggplot")

    ## Return DataFrame.
    x <- do.call(
        what = plotVolcano,
        args = c(
            args,
            list(return = "DataFrame")
        )
    )
    expect_s4_class(x, "DataFrame")
})
