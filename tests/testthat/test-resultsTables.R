context("resultsTables")

names <- c("all", "up", "down", "both")

test_that("tbl_df return", {
    x <- resultsTables(deseq, i = 1L, return = "tbl_df")
    expect_type(x, "list")
    expect_true(all(vapply(
        X = x,
        FUN = is,
        class2 = "tbl_df",
        FUN.VALUE = logical(1L)
    )))
    expect_identical(names(x), names)
})

test_that("DataFrameList return", {
    x <- resultsTables(deseq, i = 1L, return = "DataFrameList")
    expect_s4_class(x, "SimpleDataFrameList")
    expect_identical(names(x), names)
})

test_that("Extra mode handling", {
    args <- list(
        object = deseq,
        i = 1L,
        return = "tbl_df"
    )

    args[["extra"]] <- TRUE
    x <- do.call(what = resultsTables, args = args)
    expect_true(all(colnames(deseq@data) %in% colnames(x[[1L]])))

    args[["extra"]] <- FALSE
    x <- do.call(what = resultsTables, args = args)
    expect_false(all(colnames(deseq@data) %in% colnames(x[[1L]])))
})
