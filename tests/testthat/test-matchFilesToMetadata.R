context("matchFilesToMetadata")

test_that("Kebab files, snake user metadata out of order", {
    files <- file.path(
        "salmon",
        paste(seq_len(4L), "sample", LETTERS[seq_len(4L)], sep = "-"),
        "quant.sf"
    )
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
    object <- matchFilesToMetadata(files = files, metadata = metadata)
    expected <- data.frame(
        files = basename(dirname(files)),
        metadata = sort(metadata[[1L]])
    )
    expect_identical(object, expected)
})

test_that("Error on unresolvable mismatch", {
    files <- file.path(
        "salmon",
        paste("sample", seq_len(2L), sep = "-"),
        "quant.sf"
    )
    metadata <- data.frame(sampleID = "sample_1")
    expect_error(
        object = matchFilesToMetadata(files, metadata),
        regexp = "Match failure"
    )
})

test_that("Non-tximport quant file input", {
    files <- "sample-1.fastq.gz"
    metadata <- data.frame(sampleID = "sample_1")
    expect_error(
        object = matchFilesToMetadata(files, metadata),
        regexp = "Failed to detect sample name from quant file."
    )
})
