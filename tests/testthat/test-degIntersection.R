test_that("DESeqAnalysis", {
    object <- objs[["deseq"]]
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
        MoreArgs = list("object" = object),
        f = function(object, return, type) {
            expect_type(
                object = degIntersection(object, return = return),
                type = type
            )
        }
    )
})
