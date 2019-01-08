#' @name topTables
#' @inherit bioverbs::topTables
#' @inheritParams basejump::params
#' @inheritParams params
#'
#' @param n `integer(1)`.
#'   Number of genes (per direction) to report.
#'
#' @examples
#' data(deseq)
#'
#' ## DESeqAnalysis ====
#' topTables(deseq, results = 1L, n = 5L)
NULL



#' @importFrom bioverbs topTables
#' @aliases NULL
#' @export
bioverbs::topTables



.topTable <-  # nolint
    function(object, n = 10L) {
        assert(
            is(object, "tbl_df"),
            isInt(n),
            isPositive(n)
        )

        # Select minimal columns of interest.
        required <- c(
            "geneID",
            "geneName",
            "baseMean",
            "log2FoldChange",
            "padj",
            "geneBiotype"
        )
        assert(isSubset(required, colnames(object)))
        optional <- "description"
        keep <- intersect(
            x = c(required, optional),
            y = colnames(object)
        )
        object <- object[, keep, drop = FALSE]

        # Get the top rows.
        object <- head(object, n = n)

        # Sanitize optional columns first.
        if ("description" %in% colnames(object)) {
            object[["description"]] <- object[["description"]] %>%
                as.character() %>%
                # Remove symbol information in brackets.
                sub(
                    pattern = " \\[.+\\]$",
                    replacement = "",
                    x = .
                ) %>%
                # Truncate to max 50 characters.
                str_trunc(width = 50L, side = "right")
        }

        # Now we can standardize using dplyr and return.
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
            # Shorten `log2FoldChange` to `lfc` to keep column width compact.
            rename(lfc = !!sym("log2FoldChange")) %>%
            mutate_all(as.character)
    }



topTables.DESeqAnalysis <-  # nolint
    function(
        object,
        results,
        n = 10L
    ) {
        # Suppress the message about which results we're matching here,
        # otherwise it will be duplicated in the `resultsTables` call.
        suppressMessages(
            contrast <- contrastName(object, results = results)
        )
        list <- resultsTables(
            object = object,
            results = results,
            rowData = TRUE,
            counts = FALSE,
            return = "tbl_df"
        )

        # Upregulated genes.
        up <- list[["up"]]
        if (hasLength(up)) {
            show(kable(
                x = .topTable(up, n = n),
                caption = paste(contrast, "(upregulated)")
            ))
        }

        # Downregulated genes.
        down <- list[["down"]]
        if (hasLength(down)) {
            show(kable(
                x = .topTable(down, n = n),
                caption = paste(contrast, "(downregulated)")
            ))
        }

        # Invisibly return list containing the subsets.
        invisible(list(up = up, down = down))
    }



#' @rdname topTables
#' @export
setMethod(
    f = "topTables",
    signature = signature("DESeqAnalysis"),
    definition = topTables.DESeqAnalysis
)
