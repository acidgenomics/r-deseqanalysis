#' DESeq aggregate results matrix
#'
#' Generate an aggregate matrix of `DESeqResults` column values per contrast.
#'
#' @name resultsMatrix
#' @note Updated 2022-05-17.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @param rowData `logical(1)`.
#' Include row (gene) annotations, bound to the left side of the data frame.
#'
#' @param value `character(1)`.
#' Value type to return. Corresponds to supported `DESeqResults` column:
#'
#' - `log2FoldChange`: log2 fold change.\cr
#' This will return *shrunken* LFC values if they are defined.
#' - `stat`: Wald test statistic.
#' - `alpha`: Either (1) `padj`, the BH adjusted *P* value; or (2) `svalue`,
#' the s-value, when using apeglm (or ashr).
#'
#' @return
#' - `rowData = FALSE`: `matrix`.
#' - `rowData = TRUE`: `DataFrame`.
#'
#' @examples
#' data(deseq)
#'
#' ## DESeqAnalysis ====
#' x <- resultsMatrix(deseq)
#' head(x)
NULL



## Updated 2021-08-09.
`resultsMatrix,DESeqAnalysis` <- # nolint
    function(object,
             value = c("log2FoldChange", "stat", "alpha"),
             rowData = FALSE) {
        validObject(object)
        assert(isFlag(rowData))
        value <- match.arg(value)
        slotName <- .whichResults(object, value = value)
        results <- slot(object, name = slotName)
        assert(
            is.list(results),
            hasLength(results),
            hasValidNames(results)
        )
        ## Ensure we dynamically handle "padj" or "svalue", when applicable.
        value <- switch(
            EXPR = value,
            "alpha" = .alphaCol(results[[1L]]),
            value
        )
        list <- lapply(
            X = results,
            col = value,
            FUN = function(data, col) {
                assert(isSubset(col, colnames(data)))
                data[[col]]
            }
        )
        mat <- matrix(
            data = unlist(list, recursive = FALSE, use.names = FALSE),
            ncol = length(list),
            byrow = FALSE,
            dimnames = list(
                rownames(results[[1L]]),
                names(list)
            )
        )
        assert(
            identical(
                unname(results[[1L]][[value]]),
                unname(mat[, 1L, drop = TRUE])
            )
        )
        if (isTRUE(rowData)) {
            out <- .joinRowData(
                object = as(mat, "DataFrame"),
                DESeqDataSet = as(object, "DESeqDataSet")
            )
        } else {
            out <- mat
        }
        metadata2(out, which = "DESeqAnalysis") <- # nolint
            list(
                "date" = Sys.Date(),
                "packageVersion" = .pkgVersion,
                "slotName" = slotName,
                "value" = value
            )
        out
    }



## Updated 2022-05-17.
`resultsMatrix,DESeqAnalysisList` <- # nolint
    function(object, value, rowData) {
        validObject(object)
        assert(isFlag(rowData))
        value <- match.arg(value)
        slotName <- .whichResults(object, value)
        list <- Map(
            name = names(object),
            object = object,
            MoreArgs = list("value" = value),
            f = function(object, name, value) {
                ## Note that we're handling rowData below.
                m <- resultsMatrix(
                    object = object,
                    value = value,
                    rowData = FALSE
                )
                colnames(m) <- makeNames(paste(name, colnames(m)))
                m
            }
        )
        out <- do.call(what = cbind, args = list)
        if (isTRUE(rowData)) {
            out <- .joinRowData(
                object = out,
                DESeqDataSet = as(object, "DESeqDataSet")
            )
        }
        metadata2(out, which = "DESeqAnalysis") <-
            list(
                "date" = Sys.Date(),
                "packageVersion" = .pkgVersion,
                "slotName" = slotName,
                "value" = value
            )
        out
    }

.args <- c("value", "rowData")
formals(`resultsMatrix,DESeqAnalysisList`)[.args] <-
    formals(`resultsMatrix,DESeqAnalysis`)[.args]
rm(.args)



#' @rdname resultsMatrix
#' @export
setMethod(
    f = "resultsMatrix",
    signature = signature(object = "DESeqAnalysis"),
    definition = `resultsMatrix,DESeqAnalysis`
)

#' @describeIn resultsMatrix Loop across the nested `DESeqAnalysis` objects and
#' aggregate the corresponding result matrices. Note that the analysis names
#' are automatically prefixed to the column names.
#' @export
setMethod(
    f = "resultsMatrix",
    signature = signature(object = "DESeqAnalysisList"),
    definition = `resultsMatrix,DESeqAnalysisList`
)
