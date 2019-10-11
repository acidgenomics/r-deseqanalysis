## Render multiple DESeqAnalysis reports.
## Updated 2019-10-11.

library(rmarkdown)

templateFile <- "deseqanalysis.Rmd"
stopifnot(file.exists(templateFile))

## Load the DESeqAnalysis objects.
datasets <- c(
    name1 = "dds1_analysis",
    name2 = "dds2_analysis"
)
objectFiles <- file.path(
    "rds",
    Sys.Date(),
    paste0(datasets, ".rds")
)
names(objectFiles) <- names(datasets)
stopifnot(all(file.exists(objectFiles)))

outputDir <- file.path("results", Sys.Date(), "differential-expression")

invisible(mapply(
    name = names(objectFiles),
    file = objectFiles,
    FUN = function(name, file) {
        message(sprintf(
            "Rendering '%s'\nFile: %s",
            name, file
        ))
        render(
            params = list(
                title = paste("DESeq2 differential expression:", name),
                object = file,
                name = name,
                output_dir = outputDir
            ),
            input = templateFile,
            output_format = "html_document",
            output_file = paste0(name, ".html"),
            output_dir = outputDir,
            clean = TRUE,
            envir = new.env()
        )
    },
    SIMPLIFY = FALSE,
    USE.NAMES = FALSE
))
