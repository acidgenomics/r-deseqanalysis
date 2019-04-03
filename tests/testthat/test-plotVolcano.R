context("plotVolcano")

test_that("plotVolcano", {
    args <- list(object = deseq, results = 1L)

    x <- do.call(plotVolcano, args)
    expect_is(x, "ggplot")

    # Enable histograms.
    x <- do.call(
        what = plotVolcano,
        args = c(
            args,
            list(histograms = TRUE)
        )
    )
    expect_is(x, "ggplot")

    # Directional support.
    x <- do.call(
        what = plotVolcano,
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
        what = plotVolcano,
        args = c(
            args,
            list(
                direction = "down",
                sigPointColor = "green"
            )
        )
    )
    expect_is(x, "ggplot")

    # Label the top genes.
    x <- do.call(
        what = plotVolcano,
        args = c(
            args,
            list(ntop = 5L)
        )
    )
    expect_is(x, "ggplot")

    # Label specific genes.
    x <- do.call(
        what = plotVolcano,
        args = c(
            args,
            list(genes = geneNames)
        )
    )
    expect_is(x, "ggplot")

    # Return DataFrame.
    x <- do.call(
        what = plotVolcano,
        args = c(
            args,
            list(return = "DataFrame")
        )
    )
    expect_s4_class(x, "DataFrame")
})
