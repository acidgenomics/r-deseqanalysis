context("DESeqAnalysis")

test_that("DESeqAnalysis", {
    data <- DESeq2::makeExampleDESeqDataSet()
    data <- DESeq2::DESeq(data)
    expect_s4_class(data, "DESeqDataSet")

    transform <- DESeq2::varianceStabilizingTransformation(data)
    expect_s4_class(transform, "DESeqTransform")

    name <- resultsNames(data)[[2L]]
    results <- results(data, name = name)
    expect_s4_class(results, "DESeqResults")

    lfcShrink <- DESeq2::lfcShrink(dds = data, res = results, coef = 2L)
    expect_s4_class(lfcShrink, "DESeqResults")

    results <- list(results)
    names(results) <- name

    lfcShrink <- list(lfcShrink)
    names(lfcShrink) <- name

    expect_true(identical(names(results), names(lfcShrink)))

    x <- DESeqAnalysis(
        data = data,
        transform = transform,
        results = results,
        lfcShrink = lfcShrink
    )
    expect_s4_class(x, "DESeqAnalysis")
})
