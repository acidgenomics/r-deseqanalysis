#' @name topTables
#' @inherit basejump::topTables
#' @inheritParams basejump::params
#' @inheritParams params
#'
#' @param n `scalar integer`. Number of genes (per direction) to report.
#'
#' @examples
#' data(deseq)
#'
#' ## DESeqAnalysis ====
#' topTables(deseq, results = 1L, n = 5L)
NULL



#' @importFrom basejump topTables
#' @aliases NULL
#' @export
basejump::topTables



.topTable <-  # nolint
    function(object, n = 10L) {
        assert(
            is_tibble(object),
            isAnImplicitInteger(n)
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
        assert_is_subset(required, colnames(object))
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
        results = 1L,
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
        assert_is_subset(c("up", "down"), names(list))

        # Upregulated genes.
        up <- list[["up"]]
        if (has_length(up)) {
            show(kable(
                x = .topTable(up, n = n),
                caption = paste(contrast, "(upregulated)")
            ))
        }

        # Downregulated genes.
        down <- list[["down"]]
        if (has_length(down)) {
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
