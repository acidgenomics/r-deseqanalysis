context("prepareTximportFiles")

test_that("makeNames", {
    mapply(
        makeNames = eval(formals(prepareTximportFiles)[["makeNames"]]),
        names = list(
            "makeNames" = c("X001_sample_A", "X100_sample_B"),
            "snakeCase" = c("x001_sample_a", "x100_sample_b"),
            "camelCase" = c("x001SampleA", "x100SampleB")
        ),
        f = function(makeNames, names) {
            files <- c(
                file.path("salmon", "1-sample-A", "quant.sf"),
                file.path("salmon", "100-sample-B", "quant.sf")
            )
            object <- prepareTximportFiles(
                files = files,
                makeNames = makeNames,
                exists = FALSE
            )
            expect_identical(unname(object), files)
            expect_named(object, names)
        }
    )
})
