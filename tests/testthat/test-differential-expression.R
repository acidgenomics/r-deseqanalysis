context("Differential Expression")

data(deseq, envir = environment())

dds <- as(deseq, "DESeqDataSet")
vst <- as(deseq, "DESeqTransform")
res <- as(deseq, "DESeqResults")

g2s <- Gene2Symbol(dds)
geneIDs <- head(g2s[["geneID"]])
geneNames <- head(g2s[["geneName"]])



# alphaSummary =================================================================
test_that("alphaSummary : DESeqDataSet", {
    object <- dds

    # Default, no contrast specified.
    x <- alphaSummary(object)
    expect_is(x, "matrix")
    expect_type(x, "integer")
    expect_equal(
        object = x,
        expected = matrix(
            # nolint start
            data = c(
                84, 69,  42,  23, 6,
                87, 68,  31,  16, 3,
                 2,  2,   2,   2, 2,
                 0, 77, 125, 115, 0
            ),
            # nolint end
            nrow = 4L,
            ncol = 5L,
            byrow = TRUE,
            dimnames = list(
                c(
                    "LFC > 0 (up)",
                    "LFC < 0 (down)",
                    "outliers [1]",
                    "low counts [2]"
                ),
                c(1e-01, 0.5e-01, 1e-02, 1e-03, 1e-06)
            )
        )
    )

    # Contrast vector.
    expect_identical(
        object = alphaSummary(
            object = object,
            contrast = c("condition", "B", "A")
        ),
        expected = x
    )

    # Contrast name.
    expect_identical(
        object = alphaSummary(
            object = object,
            name = "condition_B_vs_A"
        ),
        expected = x
    )
})



# plotDEGHeatmap ===============================================================
test_that("plotDEGHeatmap", {
    expect_is(
        object = plotDEGHeatmap(deseq, results = 1L),
        class = "pheatmap"
    )
})



# plotDEGPCA ===================================================================
test_that("plotDEGPCA", {
    expect_is(
        object = plotDEGPCA(deseq, results = 1L),
        class = "ggplot"
    )
})



# plotMA =======================================================================
test_that("plotMA", {
    args <- list(object = deseq, results = 1L)

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

    # Directional support.
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

    # Label the top genes.
    x <- do.call(
        what = plotMA,
        args = c(args, list(ntop = 10L))
    )
    expect_is(x, "ggplot")

    # Label specific genes.
    x <- do.call(
        what = plotMA,
        args = c(
            args,
            list(genes = geneNames)
        )
    )
    expect_is(x, "ggplot")

    # DataFrame return.
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



# plotVolcano ==================================================================
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
