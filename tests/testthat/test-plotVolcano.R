args <- list("object" = objs[["deseq"]], "i" = 1L)

test_that("plotVolcano", {
    x <- do.call(what = plotVolcano, args = args)
    expect_is(x, "ggplot")
})

test_that("Enable histograms", {
    x <- do.call(
        what = plotVolcano,
        args = append(
            x = args,
            values = list("histograms" = TRUE)
        )
    )
    expect_is(x, "ggplot")
})

test_that("Directional support", {
    x <- do.call(
        what = plotVolcano,
        args = append(
            x = args,
            values = list("direction" = "up")
        )
    )
    expect_is(x, "ggplot")
    x <- do.call(
        what = plotVolcano,
        args = append(
            x = args,
            values = list("direction" = "down")
        )
    )
    expect_is(x, "ggplot")
})

test_that("Label the top genes", {
    x <- do.call(
        what = plotVolcano,
        args = append(
            x = args,
            values = list("ntop" = 5L)
        )
    )
    expect_is(x, "ggplot")
})

test_that("Label specific genes", {
    x <- do.call(
        what = plotVolcano,
        args = append(
            x = args,
            values = list("genes" = objs[["geneNames"]])
        )
    )
    expect_is(x, "ggplot")
})
