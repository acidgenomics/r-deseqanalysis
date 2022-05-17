context("degIntersection")

test_that("DESeqAnalysis", {
    Map(
        return = c(
            "matrix",
            "count",
            "ratio",
            "names"
        ),
        class = c(
            "matrix",
            "integer",
            "numeric",
            "character"
        ),
        MoreArgs = list("object" = deseq),
        f = function(object, return, class) {
            expect_is(
                object = degIntersection(object, return = return),
                class = class
            )
        }
    )
})
