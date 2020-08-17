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
#' @name apeglmResults
#' @note Updated 2020-08-17.
#'
#' @inheritParams acidroxygen::params
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
#' @param res `DESeqResults`.
#'   Results containing unshrunken LFC values, generated with `results()`.
#'
#' @seealso
#' - "Extended section on shrinkage estimators" section of DESeq2 vignette,
#'   which explains how to manually define `coef` argument which can
#'   be used with apeglm [`lfcShrink()`][DESeq2::lfcShrink].
#' - [apeglm::apeglm()].
#' - [DESeq2::lfcShrink()].
#' - [stats::model.matrix()].
#' - [DESeq2::resultsNames()].
#' - [DESeq2::DESeq()].
#'
#' @return `DESeqResults`, with apeglm adaptive shrinkage applied to fold
#'   change values.
#'
#' @examples
#' ## DESeqDataSet ====
#' if (requireNamespace("apeglm", quietly = TRUE)) {
#'     dds <- DESeq2::makeExampleDESeqDataSet(n = 1000L, m = 12L)
#'     dds$condition <- factor(rep(LETTERS[seq_len(4L)], each = 3L))
#'     dds <- DESeq2::DESeq(dds)
#'     resultsNames(dds)
#'
#'     ## Contrast C vs. B.
#'     contrast <- c(factor = "condition", numerator = "C", denominator = "B")
#'
#'     ## Unshrunken DESeqResults.
#'     res <- DESeq2::results(dds, contrast = contrast)
#'     class(res)
#'     lfcShrinkType(res)
#'
#'     ## Shrunken DESeqResults, using apeglm via `lfcShrink()`.
#'     shrink <- apeglmResults(dds, contrast = contrast)
#'     class(shrink)
#'     lfcShrinkType(shrink)
#' }
NULL



## Useful for avoiding this issue:
## type='apeglm' shrinkage only for use with 'coef'
## Updated 2020-08-17.
`apeglmResults,DESeqDataSet` <-  # nolint
    function(
            object,
            contrast,
            res,
            lfcThreshold = 0L,
            ...
        ) {
        validObject(object)
        requireNamespaces("apeglm")
        assert(
            isCharacter(resultsNames(object)),
            isCharacter(contrast),
            hasLength(contrast, n = 3L),
            isSubset(contrast[[1L]], names(colData(object))),
            is(res, "DESeqResults"),
            isNumber(lfcThreshold), isNonNegative(lfcThreshold)
        )
        parallel <- TRUE
        factor <- contrast[[1L]]
        numerator <- contrast[[2L]]
        denominator <- contrast[[3L]]
        ## Get the grouping factor defined by contrast vector.
        group <- colData(object)[[factor]]
        assert(is.factor(group))
        group <- relevel(x = group, ref = denominator)
        colData(object)[[factor]] <- group
        cli_dl(
            c("design" = paste0(as.character(design(object)), collapse = ""))
        )
        object <- DESeq(object = object, parallel = parallel)
        resultsNames <- resultsNames(object)
        ## Match the contrast to coef, based on resultsNames.
        coef <- .contrast2coef(contrast = contrast, resultsNames = resultsNames)
        ## Quiet down apeglm citation message, it's too noisy.
        suppressMessages({
            shrink <- DESeq2::lfcShrink(
                dds = object,
                coef = coef,
                res = res,
                type = "apeglm",
                lfcThreshold = lfcThreshold,
                parallel = parallel
            )
        })
        assert(
            is(shrink, "DESeqResults"),
            identical(lfcShrinkType(shrink), "apeglm"),
            identical(lfcShrinkType(res), "unshrunken"),
            identical(res[["baseMean"]], shrink[["baseMean"]]),
            !identical(res[["log2FoldChange"]], shrink[["log2FoldChange"]]),
            !identical(res[["stat"]], shrink[["stat"]]),
            identical(res[["pvalue"]], shrink[["pvalue"]]),
            identical(res[["padj"]], shrink[["padj"]]),
            identical(
                x = metadata(res)[["alpha"]],
                y = metadata(shrink)[["alpha"]]
            ),
            identical(
                x = metadata(res)[["lfcThreshold"]],
                y = metadata(shrink)[["lfcThreshold"]]
            )
        )
        shrink
    }



#' @rdname apeglmResults
#' @export
setMethod(
    f = "apeglmResults",
    signature = signature("DESeqDataSet"),
    definition = `apeglmResults,DESeqDataSet`
)
