context("degIntersection")

test_that("DESeqAnalysis", {
    ## Intersection matrix.
    object <- degIntersection(deseq, return = "matrix")
    expect_is(object, "matrix")
    ## Number of intersections.
    object <- degIntersection(deseq, return = "count")
    expect_is(object, "numeric")
    ## Intersection ratio.
    object <- degIntersection(deseq, return = "ratio")
    expect_is(object, "numeric")
})
