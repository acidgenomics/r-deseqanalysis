context("DESeqResultsTables")

data(deseq, envir = environment())



# DESeqResultsTables ===========================================================
test_that("DESeqResultsTables", {
    expect_s4_class(
        object = DESeqResultsTables(deseq),
        class = "DESeqResultsTables"
    )
})
