#' Extract or replace parts of an object
#'
#' Extract genes by row and samples by column.
#'
#' @name extract
#' @inherit base::Extract params references
#' @note Updated 2020-10-28.
#'
#' @inheritParams AcidRoxygen::params
#'
#' @return `DESeqAnalysis`.
#'
#' @examples
#' data(deseq)
#' object <- deseq
#' dim(object)
#'
#' genes <- head(rownames(object), 50L)
#' head(genes)
#' samples <- head(colnames(object), 2L)
#' head(samples)
#'
#' x <- object[genes, samples]
#' print(x)
NULL



## Updated 2020-10-28.
`extract,DESeqAnalysis` <-  # nolint
    function(
        x, i, j,
        drop = FALSE
    ) {
        validObject(x)
        assert(isFALSE(drop))
        ## Genes (rows).
        if (missing(i)) {
            i <- seq_len(nrow(x))
        }
        ## Samples (columns).
        if (missing(j)) {
            j <- seq_len(ncol(x))
        }
        ## Determine whether we should stash subset in metadata.
        if (identical(x = dim(x), y = c(length(i), length(j)))) {
            subset <- FALSE
        } else {
            subset <- TRUE
        }
        ## data (DESeqDataSet) -------------------------------------------------
        ## Extract internal DESeqDataSet.
        data1 <- slot(x, name = "data")
        data2 <- data1[i, j, drop = FALSE]
        ## Early return original object, if unmodified.
        if (identical(data1, data2)) {
            return(x)
        }
        ## transform (DESeqTransform) ------------------------------------------
        transform1 <- slot(x, name = "transform")
        transform2 <- transform1[i, j, drop = FALSE]
        ## results (DESeqResults list) -----------------------------------------
        results1 <- slot(x, name = "results")
        results2 <- lapply(
            X = results1,
            FUN = function(x) {
                x[i, , drop = FALSE]
            }
        )
        ## lfcShrink (DESeqResults list) ---------------------------------------
        lfcShrink1 <- slot(x, name = "lfcShrink")
        if (is.null(lfcShrink1)) {
            lfcShrink2 <- lfcShrink1
        } else {
            lfcShrink2 <- lapply(
                X = lfcShrink1,
                FUN = function(x) {
                    x[i, , drop = FALSE]
                }
            )
        }
        ## Return --------------------------------------------------------------
        out <- DESeqAnalysis(
            data = data2,
            transform = transform2,
            results = results2,
            lfcShrink = lfcShrink2
        )
        meta <- metadata(x)
        meta[["subset"]] <- subset
        metadata(out) <- meta
        out
    }



#' @rdname extract
#' @export
setMethod(
    f = "[",
    signature = signature(
        x = "DESeqAnalysis",
        i = "ANY",
        j = "ANY",
        drop = "ANY"  # Don't use logical here.
    ),
    definition = `extract,DESeqAnalysis`
)
