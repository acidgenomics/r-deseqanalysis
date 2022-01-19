context("matchMetadataToFiles")

test_that("Kebab files, snake user metadata out of order", {
    metadata <- data.frame(
        "sampleId" = paste(
            seq_len(4L),
            "sample",
            LETTERS[seq_len(4L)],
            sep = "_"
        ),
        "condition" = rep(LETTERS[seq_len(2L)], times = 2L),
        stringsAsFactors = FALSE
    )
    files <- file.path(
        "salmon",
        paste(seq_len(4L), "sample", LETTERS[seq_len(4L)], sep = "-"),
        "quant.sf"
    )
    files <- rev(files)
    out <- matchMetadataToFiles(metadata = metadata, files = files)
    expect_identical(
        object = out[[1L]],
        expected = c(
            "1-sample-A",
            "2-sample-B",
            "3-sample-C",
            "4-sample-D"
        )
    )
})

test_that("'prepareTximportFiles()' return mode", {
    files <- file.path("salmon", "1-sample", "quant.sf")
    metadata <- data.frame("sampleId" = "1_sample")
    out <- matchMetadataToFiles(metadata = metadata, files = files)
    expect_identical(out[[1L]], "1-sample")
    files <- prepareTximportFiles(files, exists = FALSE)
    out <- matchMetadataToFiles(metadata = metadata, files = files)
    expect_identical(out[[1L]], "X1_sample")
})

test_that("Error on unresolvable mismatch", {
    metadata <- data.frame("sampleId" = "sample_1")
    files <- file.path("salmon", "sample_2", "quant.sf")
    expect_error(
        object = matchMetadataToFiles(metadata = metadata, files = files),
        regexp = "Match failure"
    )
})

test_that("Error on length mismatch", {
    metadata <- data.frame("sampleId" = "sample_1")
    files <- file.path(
        "salmon",
        paste("sample", seq_len(2L), sep = "-"),
        "quant.sf"
    )
    expect_error(
        object = matchMetadataToFiles(metadata = metadata, files = files),
        regexp = "areSameLength"
    )
})

test_that("Error on non-tximport quant file input", {
    metadata <- data.frame("sampleId" = "sample_1")
    files <- "sample-1.fastq.gz"
    expect_error(
        object = matchMetadataToFiles(metadata = metadata, files = files),
        regexp = "quant"
    )
})
