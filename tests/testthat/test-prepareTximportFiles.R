context("prepareTximportFiles")

makeNames <- eval(formals(prepareTximportFiles)[["makeNames"]])
with_parameters_test_that(
    "makeNames", {
        files <- c(
            file.path("salmon", "1-sample-A", "quant.sf"),
            file.path("salmon", "2-sample-B", "quant.sf")
        )
        object <- prepareTximportFiles(
            files = files,
            makeNames = makeNames,
            exists = FALSE
        )
        expect_identical(unname(object), files)
        expect_identical(names(object), names)
    },
    makeNames = makeNames,
    names = list(
        makeNames = c("X1_sample_A", "X2_sample_B"),
        snakeCase = c("x1_sample_a", "x2_sample_b"),
        camelCase = c("x1SampleA", "x2SampleB")
    )
)
