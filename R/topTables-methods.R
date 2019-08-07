#' @name topTables
#' @inherit bioverbs::topTables
#' @note Updated 2019-07-30.
#'
#' @inheritParams acidroxygen::params
#' @inheritParams params
#' @param n `integer(1)`.
#'   Number of genes (per direction) to report.
#' @param ... Additional arguments.
#'
#' @examples
#' data(deseq)
#'
#' ## DESeqAnalysis ====
#' topTables(deseq, results = 1L, n = 5L)
NULL



#' @rdname topTables
#' @name topTables
#' @importFrom bioverbs topTables
#' @usage topTables(object, ...)
#' @export
NULL



## Internal functions ==========================================================
## Updated 2019-07-30.
.topTibble <-  # nolint
    function(object, n = 10L) {
        assert(
            is(object, "tbl_df"),
            isInt(n),
            isPositive(n)
        )

        ## Ensure columns are in camel case.
        object <- camelCase(object)

        ## Select minimal columns of interest.
        required <- c(
            "rowname",
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
            object[["description"]] <- object[["description"]] %>%
                as.character() %>%
                ## Remove symbol information in brackets.
                sub(
                    pattern = " \\[.+\\]$",
                    replacement = "",
                    x = .
                ) %>%
                ## Truncate to max 50 characters.
                str_trunc(width = 50L, side = "right")
        }

        ## Now we can standardize using dplyr and return.
        object %>%
            mutate(
                baseMean = round(!!sym("baseMean"), digits = 0L),
                log2FoldChange = format(
                    x = !!sym("log2FoldChange"),
                    digits = 3L,
                    scientific = FALSE
                ),
                padj = format(
                    x = !!sym("padj"),
                    digits = 3L,
                    scientific = TRUE
                )
            ) %>%
            ## Shorten `log2FoldChange` to `lfc` to keep column width compact.
            rename(lfc = !!sym("log2FoldChange")) %>%
            mutate_all(as.character)
    }



## Updated 2019-07-23.
.topKables <-  # nolint
    function(object, contrast, n) {
        assert(
            is.list(object),
            isString(contrast),
            isInt(n)
        )
        ## Upregulated genes.
        up <- object[["up"]]
        if (hasLength(up)) {
            show(kable(
                x = .topTibble(up, n = n),
                caption = paste(contrast, "(upregulated)")
            ))
        }
        ## Downregulated genes.
        down <- object[["down"]]
        if (hasLength(down)) {
            show(kable(
                x = .topTibble(down, n = n),
                caption = paste(contrast, "(downregulated)")
            ))
        }
        ## Invisibly return list containing the subsets.
        invisible(list(up = up, down = down))
    }



## DESeqResults ================================================================
## This is used in bcbioRNASeq F1000 paper.
## Updated 2019-07-30.
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
                DESeqResults = object,
                DESeqDataSet = DESeqDataSet
            )
        }
        list <- resultsTables(object, return = "tbl_df")
        contrast <- contrastName(object)
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
    signature = signature("DESeqResults"),
    definition = `topTables,DESeqResults`
)



## DESeqAnalysis ===============================================================
## Updated 2019-07-30.
`topTables,DESeqAnalysis` <-  # nolint
    function(
        object,
        results,
        n = 10L,
        lfcShrink = TRUE
    ) {
        list <- resultsTables(
            object = object,
            results = results,
            lfcShrink = lfcShrink,
            extra = TRUE,
            return = "tbl_df"
        )
        ## Suppressing the message about the contrast name we're matching here,
        ## since it will be shown in `resultsTables()` call above.
        suppressMessages(
            contrast <- contrastName(object, results = results)
        )
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
