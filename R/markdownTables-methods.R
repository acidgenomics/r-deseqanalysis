#' @name markdownTables
#' @inherit AcidGenerics::markdownTables
#' @note Updated 2022-05-24.
#'
#' @inheritParams AcidRoxygen::params
#' @inheritParams params
#' @param ... Additional arguments.
#'
#' @param contrastName `character(1)` or `NULL`.
#' Contrast name.
#'
#' @param n `integer(1)`.
#' Number of genes (per direction) to report.
#'
#' @examples
#' data(deseq)
#'
#' ## DESeqAnalysis ====
#' markdownTables(deseq, i = 1L, n = 5L)
#'
#' ## DESeqResults 'resultsTables()' return ====
#' res <- results(deseq, i = 1L)
#' resTbl <- resultsTables(res)
#' markdownTables(resTbl, n = 5L)
NULL



## Updated 2022-05-24.
.degKable <- # nolint
    function(object, caption, n = 10L) {
        assert(
            requireNamespaces("knitr"),
            is(object, "DESeqResults"),
            isString(caption),
            isInt(n),
            isPositive(n)
        )
        alphaCol <- .alphaCol(object)
        object <- as(object, "DFrame")
        object <- camelCase(object, strict = TRUE)
        required <- c("baseMean", "log2FoldChange", alphaCol)
        assert(isSubset(required, colnames(object)))
        optional <- c("broadClass", "geneName", "description")
        keep <- intersect(x = c(required, optional), y = colnames(object))
        object <- object[, keep, drop = FALSE]
        object <- head(object, n = n)
        if (isSubset("description", colnames(object))) {
            desc <- object[["description"]]
            desc <- as.character(desc)
            ## Remove gene symbol information in brackets.
            desc <- sub(
                pattern = " \\[.+\\]$",
                replacement = "",
                x = desc
            )
            maxWidth <- 50L
            desc <- ifelse(
                test = nchar(desc) > maxWidth,
                yes = paste0(
                    substring(
                        text = desc,
                        first = 1L,
                        last = maxWidth - 3L
                    ),
                    "..."
                ),
                no = desc
            )
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
        object[[alphaCol]] <- format(
            x = object[[alphaCol]],
            digits = 3L,
            scientific = TRUE
        )
        ## Shorten `log2FoldChange` to `lfc` to keep column width compact.
        colnames(object)[colnames(object) == "log2FoldChange"] <- "lfc"
        show(knitr::kable(x = as.data.frame(object), caption = caption))
        invisible(object)
    }



## Updated 2022-05-24.
`markdownTables,DESeqAnalysis` <- # nolint
    function(object, i, n = 10L) {
        markdownTables(
            object = resultsTables(object = object, i = i, extra = TRUE),
            contrastName = contrastName(object, i = i),
            n = n
        )
    }



## Updated 2022-05-24.
`markdownTables,DESeqResults` <- # nolint
    function(object,
             contrastName = NULL,
             n = 10L) {
        if (is.null(contrastName)) {
            contrastName <- tryCatch(
                expr = {
                    contrastName(object)
                },
                error = function(e) NULL
            )
        }
        markdownTables(
            object = resultsTables(object, extra = FALSE),
            contrastName = contrastName,
            n = n
        )
    }



## Updated 2023-04-28.
`markdownTables,DFrameList` <- # nolint
    function(object,
             contrastName = NULL,
             n = 10L) {
        assert(isString(contrastName, nullOK = TRUE))
        out <- list()
        if (hasLength(object[["up"]])) {
            out[["up"]] <- .degKable(
                object = object[["up"]],
                caption = ifelse(
                    test = is.null(contrastName),
                    yes = "upregulated",
                    no = paste(contrastName, "(upregulated)")
                ),
                n = n
            )
        }
        if (hasLength(object[["down"]])) {
            out[["down"]] <- .degKable(
                object = object[["down"]],
                caption = ifelse(
                    test = is.null(contrastName),
                    yes = "downregulated",
                    no = paste(contrastName, "(downregulated)")
                )
            )
        }
        invisible(out)
    }



#' @rdname markdownTables
#' @export
setMethod(
    f = "markdownTables",
    signature = signature(object = "DFrameList"),
    definition = `markdownTables,DFrameList`
)

#' @rdname markdownTables
#' @export
setMethod(
    f = "markdownTables",
    signature = signature(object = "DESeqAnalysis"),
    definition = `markdownTables,DESeqAnalysis`
)

#' @rdname markdownTables
#' @export
setMethod(
    f = "markdownTables",
    signature = signature(object = "DESeqResults"),
    definition = `markdownTables,DESeqResults`
)
