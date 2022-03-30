## This approach is inspired by bcbioRNASeq F1000 manuscript full test.

context("R Markdown")

templatesDir <- system.file(
    "rmarkdown",
    "templates",
    package = .pkgName,
    mustWork = TRUE
)

renderDir <- file.path(
    tempdir(),
    paste("render", Sys.Date(), sep = "-")
)
unlink(renderDir, recursive = TRUE)
renderDir <- initDir(renderDir)

test_that("Differential expression", {
    stem <- "differential-expression"
    input <- file.path(renderDir, paste0(stem, ".Rmd"))
    file.copy(
        from = file.path(
            templatesDir,
            stem,
            "skeleton",
            "skeleton.Rmd"
        ),
        to = input,
        overwrite = TRUE
    )
    objectFile <-
        system.file(
            "data",
            "deseq.rda",
            package = .pkgName,
            mustWork = TRUE
        )
    x <- render(
        input = input,
        params = list(
            "object" = objectFile
        ),
        clean = TRUE
    )
    outfile <- file.path(renderDir, paste0(stem, ".html"))
    expect_identical(x, outfile)
    expect_true(file.exists(outfile))
})

unlink(renderDir, recursive = TRUE)
