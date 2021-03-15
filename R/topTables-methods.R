## FIXME DESeqResultsList support?
## FIXME DESeqAnalysisList support?



#' @name topTables
#' @inherit AcidGenerics::topTables
#' @note Updated 2021-02-10.
#'
#' @inheritParams AcidRoxygen::params
#' @inheritParams params
#' @param contrast `character(1)` or `NULL`.
#'   Contrast name.
#' @param n `integer(1)`.
#'   Number of genes (per direction) to report.
#' @param ... Additional arguments.
#'
#' @examples
#' data(deseq)
#'
#' ## DESeqAnalysis ====
#' topTables(deseq, i = 1L, n = 5L)
#'
#' ## DESeqResults 'resultsTables()' list ====
#' res <- results(deseq, i = 1L)
#' resTbl <- resultsTables(res, return = "tbl_df")
#' topTables(resTbl, n = 5L)
NULL



## Updated 2019-08-20.
.topKables <-  # nolint
    function(object, contrast, n) {
        requireNamespaces("knitr")
        assert(
            is(object, "DataFrameList") || is.null(object),
            isString(contrast, nullOK = TRUE),
            isInt(n),
            isPositive(n)
        )
        ## Upregulated genes.
        up <- object[["up"]]
        if (hasLength(up)) {
            show(knitr::kable(
                x = as.data.frame(.topTable(up, n = n)),
                caption = ifelse(
                    test = is.null(contrast),
                    yes = "upregulated",
                    no = paste(contrast, "(upregulated)")
                )
            ))
        }
        ## Downregulated genes.
        down <- object[["down"]]
        if (hasLength(down)) {
            show(knitr::kable(
                x = as.data.frame(.topTable(down, n = n)),
                caption = ifelse(
                    test = is.null(contrast),
                    yes = "downregulated",
                    no = paste(contrast, "(downregulated)")
                )
            ))
        }
        ## Invisibly return list containing the subsets.
        invisible(list(up = up, down = down))
    }



## Updated 2019-08-20.
.topTable <-  # nolint
    function(object, n = 10L) {
        assert(
            is(object, "DataFrame"),
            isInt(n),
            isPositive(n)
        )
        ## Ensure columns are in camel case.
        object <- camelCase(object, strict = TRUE)
        ## Select minimal columns of interest.
        required <- c(
            "baseMean",
            "log2FoldChange",
            "padj"
        )
        assert(isSubset(required, colnames(object)))
        ## Also include optional informative columns.
        ## Use of `broadClass` is cleaner than `biotype` here.
        optional <- c(
            "broadClass",
            "geneName",
            "description"
        )
        keep <- intersect(
            x = c(required, optional),
            y = colnames(object)
        )
        object <- object[, keep, drop = FALSE]
        ## Get the top rows.
        object <- head(object, n = n)
        ## Sanitize optional columns first.
        if ("description" %in% colnames(object)) {
            desc <- object[["description"]]
            desc <- as.character(desc)
            ## Remove symbol information in brackets.
            desc <- sub(
                pattern = " \\[.+\\]$",
                replacement = "",
                x = desc
            )
            ## Truncate to max 50 characters.
            desc <- str_trunc(desc, width = 50L, side = "right")
            object[["description"]] <- desc
        }
        ## Improve number appearance.
        object[["baseMean"]] <-
            as.integer(round(object[["baseMean"]], digits = 0L))
        object[["log2FoldChange"]] <- format(
            x = object[["log2FoldChange"]],
            digits = 3L,
            scientific = FALSE
        )
        object[["padj"]] <- format(
            x = object[["padj"]],
            digits = 3L,
            scientific = TRUE
        )
        ## Shorten `log2FoldChange` to `lfc` to keep column width compact.
        colnames(object)[colnames(object) == "log2FoldChange"] <- "lfc"
        object
    }



## Updated 2020-08-05.
`topTables,DESeqAnalysis` <-  # nolint
    function(object, i, n = 10L) {
        list <- resultsTables(
            object = object,
            i = i,
            extra = TRUE,
            return = "DataFrameList"
        )
        contrast <- contrastName(object, i = i)
        .topKables(object = list, contrast = contrast, n = n)
    }



## This is used in bcbioRNASeq F1000 paper.
## Updated 2019-11-12.
`topTables,DESeqResults` <-  # nolint
    function(
        object,
        DESeqDataSet = NULL,  # nolint
        n = 10L
    ) {
        validObject(object)
        assert(isAny(DESeqDataSet, c("DESeqDataSet", "NULL")))
        if (is(DESeqDataSet, "DESeqDataSet")) {
            object <- .joinRowData(
                object = object,
                DESeqDataSet = DESeqDataSet
            )
        }
        list <- resultsTables(object, return = "DataFrameList")
        contrast <- contrastName(object)
        .topKables(
            object = list,
            contrast = contrast,
            n = n
        )
    }



## This is used in bcbioRNASeq F1000 paper.
## Updated 2019-09-17.
`topTables,list` <-  # nolint
    function(object, n = 10L, contrast = NULL) {
        assert(
            isSubset(c("down", "up"), names(object)),
            is(object[[1L]], "tbl_df"),
            isSubset(
                x = c("rowname", "baseMean", "log2FoldChange", "padj"),
                y = colnames(object[[1L]])
            )
        )
        ## Coerce tbl_df list to DataFrameList.
        list <- DataFrameList(lapply(
            X = object,
            FUN = function(x) {
                x <- as(x, "DataFrame")
                x <- as(x, "DESeqResults")
                x
            }
        ))
        .topKables(
            object = list,
            contrast = contrast,
            n = n
        )
    }



#' @rdname topTables
#' @export
setMethod(
    f = "topTables",
    signature = signature("DESeqAnalysis"),
    definition = `topTables,DESeqAnalysis`
)



#' @rdname topTables
#' @export
setMethod(
    f = "topTables",
    signature = signature("DESeqResults"),
    definition = `topTables,DESeqResults`
)



#' @describeIn topTables Legacy support for `tbl_df` list returned from
#'   `resultsTables()`. This method is still supported because it is used in the
#'   F1000 v2 workflow paper. Otherwise, we now recommend using the
#'   `DESeqAnalysis` method directly.
#' @export
setMethod(
    f = "topTables",
    signature = signature("list"),
    definition = `topTables,list`
)
