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
        object = plotDEGHeatmap(deseq),
        class = "pheatmap"
    )
})



# plotDEGPCA ===================================================================
test_that("plotDEGPCA", {
    expect_is(
        object = plotDEGPCA(deseq),
        class = "ggplot"
    )
})



# plotMA =======================================================================
with_parameters_test_that(
    "plotMA", {
        x <- plotMA(object)
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
        x <- plotMA(
            object = object,
            direction = "up",
            sigPointColor = "red"
        )
        expect_is(x, "ggplot")
        x <- plotMA(
            object = object,
            direction = "down",
            sigPointColor = "green"
        )
        expect_is(x, "ggplot")

        # Label the top genes.
        args <- list(object = object, ntop = 10L)
        if (!is(object, "DESeqAnalysis")) {
            args[["gene2symbol"]] <- g2s
        }
        x <- do.call(what = plotMA, args = args)
        expect_is(x, "ggplot")

        # Label specific genes.
        args <- list(object = object, genes = geneNames)
        if (!is(object, "DESeqAnalysis")) {
            args[["gene2symbol"]] <- g2s
        }
        x <- do.call(what = plotMA, args = args)
        expect_is(x, "ggplot")

        # DataFrame return.
        x <- plotMA(object, return = "DataFrame")
        expect_s4_class(x, "DataFrame")
        expect_true("isDE" %in% colnames(x))
    },
    object = list(
        DESeqAnalysis = deseq,
        DESeqResults = res
    )
)



# plotVolcano ==================================================================
with_parameters_test_that(
    "plotVolcano", {
        x <- plotVolcano(object)
        expect_is(x, "ggplot")

        # Enable histograms.
        x <- plotVolcano(object, histograms = TRUE)
        expect_is(x, "ggplot")

        # Directional support.
        x <- plotVolcano(
            object = object,
            direction = "up",
            sigPointColor = "red"
        )
        expect_is(x, "ggplot")
        x <- plotVolcano(
            object = object,
            direction = "down",
            sigPointColor = "green"
        )
        expect_is(x, "ggplot")

        # Label the top genes.
        args <- list(object = object, ntop = 5L)
        if (!is(object, "DESeqAnalysis")) {
            args[["gene2symbol"]] <- g2s
        }
        x <- do.call(what = plotVolcano, args = args)
        expect_is(x, "ggplot")

        # Label specific genes.
        args <- list(object = object, genes = geneNames)
        if (!is(object, "DESeqAnalysis")) {
            args[["gene2symbol"]] <- g2s
        }
        x <- do.call(what = plotVolcano, args = args)
        expect_is(x, "ggplot")

        # Return DataFrame.
        x <- plotVolcano(object, return = "DataFrame")
        expect_s4_class(x, "DataFrame")
    },
    object = list(
        DESeqAnalysis = deseq,
        DESeqResults = res
    )
)
