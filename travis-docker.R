rcmdcheck::rcmdcheck(args = "--no-manual")
file.create(
    file.path(
        "inst",
        "rmarkdown",
        "templates",
        "differential-expression",
        "skeleton",
        c("_footer.Rmd", "_header.Rmd", "_links.Rmd")
    )
)
BiocCheck::BiocCheck()
lintr::lint_package()
