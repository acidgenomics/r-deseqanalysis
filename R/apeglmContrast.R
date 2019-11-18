#' Run a quick pairwise contrast using lfcShrink with apeglm
#'
#' Wrapper function that helps set up [`lfcShrink()`][DESeq2::lfcShrink] to
#' shrink LFC values for a pairwise contrast via apeglm, without having to
#' manually relevel factor reference levels to use the required `coef` argument.
#'
#' Dynamically sets reference factor levels, as recommended by DESeq2 vignette.
#' Matches `contrast` input internally to corresponding `coef` corresponding
#' to values in [`resultsNames()`][DESeq2::resultsNames].
#'
#' Runs [`nbinomWaldTest()`][DESeq2::nbinomWaldTest] via
#' [`DESeq()`][DESeq2::DESeq], followed by [`lfcShrink()`][DESeq2::lfcShrink].
#'
#' @export
#' @note Updated 2019-11-18.
#'
#' @param dds `DESeqDataSet`.
#' @param contrast `character(3)`.
#'   Pairwise contrast vector:
#'
#'   1. `factor`: Grouping factor. Corresponds to column name in
#'      [`colData()`][SummarizedExperiment::colData].
#'   2. `numerator`: Numerator samples.
#'   3. `denominator`: Denominator samples.
#'
#'   Numerator and denominator values correspond to grouping factor column.
#'   See [`results()`] for details. Note that we're intentionally being more
#'   strict about the input format here.
#' @param ... Passes to [`lfcShrink()`][DESeq2::lfcShrink], with
#'   `type = "apeglm"` and `coef` automatically defined. Optionally can pass
#'   unshrunken `DESeqResults` via `res` argument here and this will set
#'   `alpha`, `lfcThreshold` automatically.
#'
#' @seealso
#' - "Extended section on shrinkage estimators" section of DESeq2 vignette,
#'   which explains how to manually define `coef` argument which can
#'   be used with apeglm [`lfcShrink()`][DESeq2::lfcShrink].
#' - [DESeq2::lfcShrink()].
#' - [apeglm::apeglm()].
#' - [stats::model.matrix()].
#' - [DESeq2::resultsNames()].
#' - [DESeq2::DESeq()].
#'
#' @return `DESeqResults`, with apeglm adaptive shrinkage applied to fold
#'   change values.
#'
#' @examples
#' dds <- DESeq2::makeExampleDESeqDataSet(n = 1000L, m = 12L)
#' dds$condition <- factor(rep(LETTERS[seq_len(4L)], each = 3L))
#' dds <- DESeq2::DESeq(dds)
#' resultsNames(dds)
#'
#' ## Contrast C vs. B.
#' contrast <- c(factor = "condition", numerator = "C", denominator = "B")
#'
#' ## Unshrunken DESeqResults.
#' res <- results(dds, contrast = contrast)
#' class(res)
#' lfcShrinkType(res)
#'
#' ## Shrunken DESeqResults, using apeglm via `lfcShrink()`.
#' if (requireNamespace("apeglm", quietly = TRUE)) {
#'     shrink <- apeglmContrast(dds = dds, contrast = contrast)
#'     class(shrink)
#'     lfcShrinkType(shrink)
#' }
apeglmContrast <- function(dds, contrast, ...) {
    validObject(dds)
    dots <- list(...)
    assert(
        requireNamespace("apeglm", quietly = TRUE),
        areDisjointSets(c("coef", "type"), names(dots)),
        isCharacter(contrast),
        hasLength(contrast, n = 3L),
        isSubset(contrast[[1L]], names(colData(dds))),
        isCharacter(resultsNames(dds))
    )
    factor <- contrast[[1L]]
    numerator <- contrast[[2L]]
    denominator <- contrast[[3L]]
    ## Get the grouping factor defined by contrast vector.
    group <- colData(dds)[[factor]]
    assert(is.factor(group))
    group <- relevel(x = group, ref = denominator)
    colData(dds)[[factor]] <- group
    message(sprintf(
        fmt = "Design: %s",
        paste0(as.character(design(dds)), collapse = "")
    ))
    dds <- DESeq(dds)
    resultsNames <- resultsNames(dds)
    ## Match the contrast to coef, based on resultsNames.
    coef <- .contrast2coef(contrast = contrast, resultsNames = resultsNames)
    ## Quiet down about citation, it's too noisy.
    suppressMessages(
        shrink <- lfcShrink(
            dds = dds,
            type = "apeglm",
            coef = coef,
            ...
        )
    )
    assert(
        is(shrink, "DESeqResults"),
        identical(lfcShrinkType(shrink), "apeglm")
    )
    ## Check that unshruken DESeqResults matches, if user passes in.
    if (is(dots[["res"]], "DESeqResults")) {
        res <- dots[["res"]]
        assert(
            identical(
                x = lfcShrinkType(res),
                y = "unshrunken"
            ),
            identical(
                x = metadata(res)[["alpha"]],
                y = metadata(shrink)[["alpha"]]
            ),
            identical(
                x = metadata(res)[["lfcThreshold"]],
                y = metadata(shrink)[["lfcThreshold"]]
            ),
            identical(
                x = res[["baseMean"]],
                y = shrink[["baseMean"]]
            ),
            !identical(
                x = res[["log2FoldChange"]],
                y = shrink[["log2FoldChange"]]
            ),
            !identical(
                x = res[["stat"]],
                y = shrink[["stat"]]
            ),
            identical(
                x = res[["pvalue"]],
                y = shrink[["pvalue"]]
            ),
            identical(
                x = res[["padj"]],
                y = shrink[["padj"]]
            )
        )
    }
    ## Return.
    shrink
}



#' Map contrast vector to coefficient
#'
#' @note Updated 2019-09-17.
#' @noRd
.contrast2coef <- function(contrast, resultsNames) {
    assert(
        isCharacter(contrast),
        hasLength(contrast, n = 3L),
        isCharacter(resultsNames)
    )
    factor <- contrast[[1L]]
    numerator <- contrast[[2L]]
    denominator <- contrast[[3L]]
    coef <- match(
        x = paste(factor, numerator, "vs", denominator, sep = "_"),
        table = resultsNames
    )
    assert(isInt(coef), !is.na(coef))
    message(sprintf("Contrast: %s\nCoef: %d", resultsNames[[coef]], coef))
    coef
}
