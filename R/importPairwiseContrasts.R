#' Import pairwise contrasts from a file
#'
#' @export
#' @note Updated 2019-09-09.
#'
#' @inheritParams AcidRoxygen::params
#'
#' @param groupCol `character(1)`.
#' Group column name in `colData()` of
#' `DESeqDataSet`. Corresponds to value in `design()`.
#'
#' @param numeratorCol `character(1)`.
#' Numerator column name in contrasts file.
#'
#' @param denominatorCol `character(1)`.
#' Denominator column name in contrasts file.
#'
#' @param namesCol `character(1)`.
#' Column in contrasts file to used to define the names of the contrast list.
#' Values will be sanitized using `snakeCase()`.
#'
#' @seealso `DESeq2::results()`.
#'
#' @return `list`.
#' Named list of pairwise character vectors containing: "group", "numerator",
#' "denominator".
#'
#' @examples
#' file <- system.file("extdata", "contrasts.csv", package = "DESeqAnalysis")
#' x <- importPairwiseContrasts(file)
#' print(x)
importPairwiseContrasts <- function(file,
                                    groupCol = "group",
                                    numeratorCol = "numerator",
                                    denominatorCol = "denominator",
                                    namesCol = "description") {
    assert(
        isAFile(file),
        isString(groupCol),
        isString(numeratorCol),
        isString(denominatorCol)
    )
    data <- import(file)
    assert(
        isSubset(
            x = c(numeratorCol, denominatorCol, namesCol),
            y = colnames(data)
        )
    )
    list <- mapply(
        numerator = data[["numerator"]],
        denominator = data[["denominator"]],
        MoreArgs = list("group" = groupCol),
        FUN = function(group, numerator, denominator) {
            c(
                "group" = group,
                "numerator" = numerator,
                "denominator" = denominator
            )
        },
        SIMPLIFY = FALSE,
        USE.NAMES = FALSE
    )
    names(list) <- snakeCase(data[[namesCol]])
    list
}
