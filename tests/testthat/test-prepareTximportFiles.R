test_that("salmon", {
    dir <- tempdir2()
    samples <- c("1-sample-A", "2-sample-B")
    lapply(X = file.path(dir, samples), FUN = initDir)
    file.create(file.path(dir, samples, "quant.sf"))
    files <- prepareTximportFiles(dir = dir, type = "salmon")
    expect_identical(unique(basename(files)), "quant.sf")
    expect_identical(
        object = names(files),
        expected = c("X1_sample_A", "X2_sample_B")
    )
    unlink2(dir)
})

test_that("kallisto", {
    dir <- tempdir2()
    samples <- c("1-sample-A", "2-sample-B")
    lapply(X = file.path(dir, samples), FUN = initDir)
    file.create(file.path(dir, samples, "abundance.h5"))
    files <- prepareTximportFiles(dir = dir, type = "kallisto")
    expect_identical(unique(basename(files)), "abundance.h5")
    expect_identical(
        object = names(files),
        expected = c("X1_sample_A", "X2_sample_B")
    )
    unlink2(dir)
})
