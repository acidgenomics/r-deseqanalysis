## """
## Render multiple DESeqAnalysis reports.
## Updated 2022-05-17.
## """

## nolint start
suppressPackageStartupMessages({
library(rmarkdown)
})
## nolint end
templateFile <- "deseqanalysis.Rmd"
stopifnot(file.exists(templateFile))
datasets <- c(
    "name1" = "deseqanalysis1",
    "name2" = "deseqanalysis2"
)
objectFiles <- file.path(
    "rds",
    Sys.Date(),
    paste0(datasets, ".rds")
)
names(objectFiles) <- names(datasets)
stopifnot(all(file.exists(objectFiles)))
outputDir <- file.path("results", Sys.Date(), "differential-expression")
invisible(Map(
    name = names(objectFiles),
    file = objectFiles,
    f = function(name, file) {
        message(sprintf("Rendering '%s'\nFile: %s", name, file))
        render(
            params = list(
                "title" = paste("DESeq2 differential expression:", name),
                "object" = file,
                "name" = name,
                "output_dir" = outputDir
            ),
            input = templateFile,
            output_format = "html_document",
            output_file = paste0(name, ".html"),
            output_dir = outputDir,
            clean = TRUE,
            envir = new.env()
        )
    }
))
