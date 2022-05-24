names <- c("all", "up", "down", "both")

test_that("DataFrameList return", {
    object <- objs[["deseq"]]
    x <- resultsTables(object, i = 1L)
    expect_s4_class(x, "SimpleDataFrameList")
    expect_s4_class(x[[1L]], "DESeqResults")
    expect_named(x, names)
})

test_that("Extra mode handling", {
    object <- objs[["deseq"]]
    args <- list(
        "object" = object,
        "i" = 1L
    )
    args[["extra"]] <- TRUE
    x <- do.call(what = resultsTables, args = args)
    expect_true(all(colnames(object@data) %in% colnames(x[[1L]])))
    args[["extra"]] <- FALSE
    x <- do.call(what = resultsTables, args = args)
    expect_false(all(colnames(object@data) %in% colnames(x[[1L]])))
})
