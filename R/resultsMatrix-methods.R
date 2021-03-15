#' DESeq aggregate results matrix
#'
#' Generate an aggregate matrix of `DESeqResults` column values per contrast.
#'
#' @name resultsMatrix
#' @note Updated 2021-03-15.
#'
#' @param object `DESeqAnalysis`.
#' @param value `character(1)`.
#'   Value type to return. Corresponds to supported `DESeqResults` column:
#'
#'   - `log2FoldChange`: log2 fold change.\cr
#'     This will return *shrunken* LFC values if they are defined.
#'   - `stat`: Wald test statistic.
#'   - `padj`: BH adjusted *P* value.
#' @param rowData `logical(1)`.
#'   Include row (gene) annotations, bound to the left side of the data frame.
#' @param ... Additional arguments.
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



## Updated 2021-03-15.
`resultsMatrix,DESeqAnalysis` <-  # nolint
    function(
        object,
        value = c("log2FoldChange", "stat", "padj"),
        rowData = FALSE
    ) {
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
        list <- lapply(
            X = results,
            col = value,
            FUN = function(data, col) data[[col]]
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
        metadata2(out, which = "DESeqAnalysis") <-
            list(
                "date" = Sys.Date(),
                "packageVersion" = .pkgVersion,
                "slotName" = slotName,
                "value" = value
            )
        out
    }



#' @rdname resultsMatrix
#' @export
setMethod(
    f = "resultsMatrix",
    signature = signature("DESeqAnalysis"),
    definition = `resultsMatrix,DESeqAnalysis`
)



## Updated 2019-10-24.
`resultsMatrix,DESeqAnalysisList` <-  # nolint
    function(object, value, rowData) {
        validObject(object)
        assert(isFlag(rowData))
        value <- match.arg(value)
        slotName <- .whichResults(object, value)
        list <- mapply(
            name = names(object),
            object = object,
            MoreArgs = list(value = value),
            FUN = function(object, name, value) {
                ## Note that we're handling rowData below.
                m <- resultsMatrix(
                    object = object,
                    value = value,
                    rowData = FALSE
                )
                colnames(m) <- makeNames(paste(name, colnames(m)))
                m
            },
            SIMPLIFY = FALSE,
            USE.NAMES = TRUE
        )
        out <- do.call(what = cbind, args = list)
        if (isTRUE(rowData)) {
            out <- .joinRowData(
                object = out,
                DESeqDataSet = as(object, "DESeqDataSet")
            )
        }
        metadata2(out, which = "DESeqAnalysis") <- list(
            version = packageVersion(packageName()),
            date = Sys.Date(),
            slotName = slotName,
            value = value
        )
        out
    }

args <- c("value", "rowData")
formals(`resultsMatrix,DESeqAnalysisList`)[args] <-
    formals(`resultsMatrix,DESeqAnalysis`)[args]



#' @describeIn resultsMatrix Loop across the nested `DESeqAnalysis` objects and
#'   aggregate the corresponding result matrices. Note that the analysis names
#'   are automatically prefixed to the column names.
#' @export
setMethod(
    f = "resultsMatrix",
    signature = signature("DESeqAnalysisList"),
    definition = `resultsMatrix,DESeqAnalysisList`
)
