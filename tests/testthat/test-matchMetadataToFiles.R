context("matchMetadataToFiles")

test_that("Kebab files, snake user metadata out of order", {
    metadata <- data.frame(
        sampleID = paste(
            seq_len(4L),
            "sample",
            LETTERS[seq_len(4L)],
            sep = "_"
        ),
        condition = rep(LETTERS[seq_len(2L)], times = 2L)
    )
    files <- file.path(
        "salmon",
        paste(seq_len(4L), "sample", LETTERS[seq_len(4L)], sep = "-"),
        "quant.sf"
    )
    files <- rev(files)
    object <- matchMetadataToFiles(metadata = metadata, files = files)
    expected <- data.frame(
        metadata = c(
            "1_sample_A",
            "2_sample_B",
            "3_sample_C",
            "4_sample_D"
        ),
        files = c(
            "1-sample-A",
            "2-sample-B",
            "3-sample-C",
            "4-sample-D"
        )
    )
    expect_identical(object, expected)
})

test_that("Error on unresolvable mismatch", {
    metadata <- data.frame(sampleID = "sample_1")
    files <- file.path("salmon", "sample_2", "quant.sf")
    expect_error(
        object = matchMetadataToFiles(metadata = metadata, files = files),
        regexp = "Match failure"
    )
})

test_that("Error on length mismatch", {
    metadata <- data.frame(sampleID = "sample_1")
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
    metadata <- data.frame(sampleID = "sample_1")
    files <- "sample-1.fastq.gz"
    expect_error(
        object = matchMetadataToFiles(metadata = metadata, files = files),
        regexp = "Failed to detect sample name from quant file."
    )
})
