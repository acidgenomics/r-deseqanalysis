test_that("list", {
    object <- objs[["deseq"]]
    x <- DESeqAnalysisList(
        object = list(
            "object1" = object,
            "object2" = object
        )
    )
    expect_s4_class(x, "DESeqAnalysisList")
})
