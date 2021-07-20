context("R Markdown")

test_that("Differential expression", {
    skeleton <- system.file(
        "rmarkdown",
        "templates",
        "differential-expression",
        "skeleton",
        "skeleton.Rmd",
        package = .pkgName,
        mustWork = TRUE
    )
    input <- tempfile(
        pattern = "render",
        tmpdir = tempdir(),
        fileext = ".Rmd"
    )
    file.copy(from = skeleton, to = input, overwrite = TRUE)
    out <- rmarkdown::render(
        input = input,
        output_format = "html_document",
        clean = TRUE,
        params = list(
            "object" = system.file(
                "data",
                "deseq.rda",
                package = .pkgName,
                mustWork = TRUE
            )
        ),
        quiet = TRUE
    )
    expect_true(file.exists(out))
})
