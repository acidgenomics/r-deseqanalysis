test_that("DESeqAnalysis", {
    Map(
        return = c(
            "matrix",
            "count",
            "ratio",
            "names"
        ),
        type = c(
            "logical",
            "integer",
            "double",
            "character"
        ),
        MoreArgs = list("object" = objs[["deseq"]]),
        f = function(object, return, type) {
            expect_type(
                object = degIntersection(object, return = return),
                type = type
            )
        }
    )
})
