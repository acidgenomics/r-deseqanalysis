context("DESeqAnalysis")

test_that("DESeqAnalysis", {
    data <- DESeq2::makeExampleDESeqDataSet()
    rowRanges <- AcidGenomes::emptyRanges(names = rownames(data))
    mcols(rowRanges)[["geneId"]] <- paste0("id", seq_len(length(rowRanges)))
    mcols(rowRanges)[["geneName"]] <- paste0("name", seq_len(length(rowRanges)))
    rowRanges(data) <- rowRanges
    data <- DESeq2::DESeq(data)
    transform <- DESeq2::varianceStabilizingTransformation(data)
    name <- resultsNames(data)[[2L]]
    results <- results(data, name = name)
    expect_s4_class(results, "DESeqResults")
    lfcShrink <- DESeq2::lfcShrink(dds = data, res = results, coef = 2L)
    expect_s4_class(lfcShrink, "DESeqResults")
    results <- list(results)
    names(results) <- name
    lfcShrink <- list(lfcShrink)
    names(lfcShrink) <- name
    x <- DESeqAnalysis(
        data = data,
        transform = transform,
        results = results,
        lfcShrink = lfcShrink
    )
    expect_s4_class(x, "DESeqAnalysis")
})
