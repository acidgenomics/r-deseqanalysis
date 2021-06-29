#' @name resultsTables
#' @inherit AcidGenerics::resultsTables
#' @note Updated 2021-06-28.
#'
#' @inheritParams params
#' @inheritParams results
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @details
#' Generate tables summarizing the differential expression, with subsets for
#' differentially expressed genes (DEGs). DEG tables (i.e. everything except the
#' `all` table), are arranged by adjusted *P* value.
#'
#' @note It is generally recommended to not apply post hoc log fold change
#'   cutoffs. If a specific effect size is desired, instead run
#'   `DESeq2::results()` using the `lfcThreshold` parameter. Refer to the DESeq2
#'   documentation and vignette for details.
#'
#' @section Tables:
#'
#' - `all`: All genes, including genes without an adjusted *P* value. This table
#'   is unmodified, and the rows have not been re-arranged or subset. It is
#'   suitable for gene set enrichment analysis (GSEA).
#' - `up`: Upregulated genes.
#' - `down`: Downregulated genes.
#' - `both`: Bidirectional DEGs (up- and down-regulated). This table can be
#'   used for overrepresentation testing but should NOT be used for GSEA.
#'
#' @param return `character(1)`.
#'   Type of data frame to return as a list.
#'   Uses `match.arg()`.
#'
#'   - `DataFrameList`: Returns `DataFrameList` with row names.
#'   - `tbl_df`: Returns `list` of `tbl_df` containing `"rowname"` column.
#'
#' @return `list`.
#' Named list containing subsets of `DESeqResults`.
#'
#' @examples
#' data(deseq)
#'
#' ## DESeqAnalysis ====
#' x <- resultsTables(deseq, i = 1L)
#' names(x)
NULL



.args <- list(
    "return" = c("tbl_df", "DataFrameList")
)



## Updated 2021-03-15.
`resultsTables,DESeqAnalysis` <-  # nolint
    function(
        object,
        i,
        alphaThreshold = NULL,
        lfcThreshold = NULL,
        baseMeanThreshold = NULL,
        extra = TRUE,
        return
    ) {
        validObject(object)
        if (is.null(baseMeanThreshold)) {
            baseMeanThreshold <- baseMeanThreshold(object)
        }
        resultsTables(
            object = results(object = object, i = i, extra = extra),
            alphaThreshold = ifelse(
                test = is.null(alphaThreshold),
                yes = alphaThreshold(object),
                no = alphaThreshold
            ),
            lfcThreshold = ifelse(
                test = is.null(lfcThreshold),
                yes = lfcThreshold(object),
                no = lfcThreshold
            ),
            baseMeanThreshold = ifelse(
                test = is.null(baseMeanThreshold),
                yes = baseMeanThreshold(object),
                no = baseMeanThreshold
            ),
            return = match.arg(return)
        )
    }

formals(`resultsTables,DESeqAnalysis`)[names(.args)] <- .args



## Updated 2021-06-29.
`resultsTables,DESeqResults` <-  # nolint
    function(
        object,
        alphaThreshold = NULL,
        baseMeanThreshold = NULL,
        lfcThreshold = NULL,
        return
    ) {
        validObject(object)
        both <- deg(
            object = object,
            direction = "both",
            alphaThreshold = alphaThreshold,
            baseMeanThreshold = baseMeanThreshold,
            lfcThreshold = lfcThreshold
        )
        if (!hasLength(both)) {
            out <- list("all" = object)
        } else {
            up <- deg(
                object = object,
                direction = "up",
                alphaThreshold = alphaThreshold,
                baseMeanThreshold = baseMeanThreshold,
                lfcThreshold = lfcThreshold
            )
            down <- deg(
                object = object,
                direction = "down",
                alphaThreshold = alphaThreshold,
                baseMeanThreshold = baseMeanThreshold,
                lfcThreshold = lfcThreshold
            )
            out <- list(
                "all" = object,
                "up" = object[up, , drop = FALSE],
                "down" = object[down, , drop = FALSE],
                "both" = object[both, , drop = FALSE]
            )
            out <- Filter(f = hasRows, x = out)
        }
        switch(
            EXPR = match.arg(return),
            "DataFrameList" = DataFrameList(out),
            "tbl_df" = lapply(out, as_tibble)
        )
    }

formals(`resultsTables,DESeqResults`)[names(.args)] <- .args



rm(.args)



#' @rdname resultsTables
#' @export
setMethod(
    f = "resultsTables",
    signature = signature("DESeqAnalysis"),
    definition = `resultsTables,DESeqAnalysis`
)



#' @rdname resultsTables
#' @export
setMethod(
    f = "resultsTables",
    signature = signature("DESeqResults"),
    definition = `resultsTables,DESeqResults`
)
