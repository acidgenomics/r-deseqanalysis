#' DESeqResults matrix
#'
#' Generate an aggregate matrix of DESeqResults values.
#'
#' @name resultsMatrix
#' @note Updated 2019-10-23.
#'
#' @param object `DESeqAnalysis`.
#' @param value `character(1)`.
#'   Value type to return. Corresponds to supported `DESeqResults` column:
#'
#'   - `log2FoldChange`: log2 fold change.\cr
#'     This will return *shrunken* LFC values if they are defined.
#'   - `stat`: Wald test statistic.
#'   - `padj`: BH adjusted *P* value.
#' @param ... Additional arguments.
#'
#' @return `matrix`.
#'
#' @examples
#' data(deseq)
#' x <- resultsMatrix(deseq)
#' head(x)
NULL



#' @rdname resultsMatrix
#' @name resultsMatrix
#' @importFrom bioverbs resultsMatrix
#' @usage resultsMatrix(object, ...)
#' @export
NULL



## Updated 2019-10-23.
`resultsMatrix,DESeqAnalysis` <-  # nolint
    function(
        object,
        value = c("log2FoldChange", "stat", "padj", "baseMean")
    ) {
        validObject(object)
        value <- match.arg(value)
        ## Get appropriate list of `DESeqResults`. Use the shrunken LFC values,
        ## if defined. Otherwise, just pull values from `results()` return.
        if (
            identical(value, "log2FoldChange") &&
            hasLength(slot(object, "lfcShrink"))
        ) {
            slotName <- "lfcShrink"
        } else {
            slotName <- "results"
        }
        message(sprintf(
            "Generating '%s' results matrix from '%s' slot.",
            value, slotName
        ))
        results <- slot(object, name = slotName)
        assert(
            is.list(results),
            length(results) > 0L,
            hasValidNames(results)
        )
        list <- lapply(
            X = results,
            col = value,
            FUN = function(data, col) data[[col]]
        )
        unlist <- unlist(list, recursive = FALSE, use.names = FALSE)
        mat <- matrix(
            data = unlist,
            ncol = length(list),
            byrow = FALSE,
            dimnames = list(
                rownames(results[[1L]]),
                names(list)
            )
        )
        ## Double check that our unlist operation is correct.
        assert(
            identical(
                unname(results[[1L]][[value]]),
                unname(mat[, 1L, drop = TRUE])
            )
        )
        ## Stash useful metadata in the object.
        attr(mat, which = "DESeqAnalysis") <-
            list(
                version = packageVersion("DESeqAnalysis"),
                date = Sys.Date(),
                slotName = slotName,
                value = value
            )
        mat
    }



#' @rdname resultsMatrix
#' @export
setMethod(
    f = "resultsMatrix",
    signature = signature("DESeqAnalysis"),
    definition = `resultsMatrix,DESeqAnalysis`
)



## Updated 2019-10-23.
`resultsMatrix,DESeqAnalysisList` <-  # nolint
    function(object, value) {
        validObject(object)
        value <- match.arg(value)
        list <- mapply(
            name = names(object),
            object = object,
            MoreArgs = list(value = value),
            FUN = function(object, name, value) {
                m <- resultsMatrix(object = object, value = value)
                colnames(m) <- makeNames(paste(name, colnames(m)))
                m
            },
            SIMPLIFY = FALSE,
            USE.NAMES = TRUE
        )
        do.call(what = cbind, args = list)
    }

formals(`resultsMatrix,DESeqAnalysisList`)[["value"]] <-
    formals(`resultsMatrix,DESeqAnalysis`)[["value"]]



#' @describeIn resultsMatrix Loops across the nested `DESeqAnalysis` objects and
#'   gets the corresponding result matrices.
#' @export
setMethod(
    f = "resultsMatrix",
    signature = signature("DESeqAnalysisList"),
    definition = `resultsMatrix,DESeqAnalysisList`
)
