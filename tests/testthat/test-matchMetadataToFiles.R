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
        files = basename(dirname(files)),
        metadata = sort(metadata[[1L]])
    )
    expect_identical(object, expected)
})

test_that("Error on unresolvable mismatch", {
    metadata <- data.frame(sampleID = "sample_1")
    files <- file.path(
        "salmon",
        paste("sample", seq_len(2L), sep = "-"),
        "quant.sf"
    )
    expect_error(
        object = matchMetadataToFiles(metadata = metadata, files = files),
        regexp = "Match failure"
    )
})

test_that("Non-tximport quant file input", {
    metadata <- data.frame(sampleID = "sample_1")
    files <- "sample-1.fastq.gz"
    expect_error(
        object = matchMetadataToFiles(metadata = metadata, files = files),
        regexp = "Failed to detect sample name from quant file."
    )
})
