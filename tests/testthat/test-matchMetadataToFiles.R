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
    metadata <- metadata[nrow(metadata):1L, ]
    files <- file.path(
        "salmon",
        paste(seq_len(4L), "sample", LETTERS[seq_len(4L)], sep = "-"),
        "quant.sf"
    )
    object <- matchMetadataToFiles(metadata = metadata, files = files)
    expected <- data.frame(
        metadata = c(
            "4_sample_D",
            "3_sample_C",
            "2_sample_B",
            "1_sample_A"
        ),
        files = c(
            "4-sample-D",
            "3-sample-C",
            "2-sample-B",
            "1-sample-A"
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
