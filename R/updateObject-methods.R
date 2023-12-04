#' Update object
#'
#' @name updateObject
#' @note Updated 2022-03-09.
#'
#' @section `DESeqAnalysis`:
#'
#' In the v0.1.8 update, we improved the class to contain `Annotated` virtual
#' class, which adds support for the `metadata()` slot. Objects saved by older
#' package versions must be updated to account for this change.
#'
#' @inheritParams AcidRoxygen::params
#'
#' @return Modified object.
#'
#' @examples
#' data(deseq)
#'
#' ## DESeqAnalysis ====
#' updateObject(deseq)
NULL



## Updated 2020-08-04.
`updateObject,DESeqAnalysis` <- # nolint
    function(object, ..., verbose = FALSE) {
        assert(isFlag(verbose))
        if (!isTRUE(.hasSlot(object, "metadata"))) {
            if (isTRUE(verbose)) {
                alertWarning(paste(
                    "Legacy object < 0.1.8 detected.",
                    "Updating to support metadata slot.",
                    sep = "\n"
                ))
            }
        }
        out <- DESeqAnalysis(
            data = object@data,
            transform = object@transform,
            results = object@results,
            lfcShrink = object@lfcShrink
        )
        if (!identical(names(metadata(object)), names(metadata(out)))) {
            diff <- setdiff(names(metadata(object)), names(metadata(out)))
            meta <- c(metadata(out), metadata(object)[diff])
            metadata(out) <- meta
        }
        out
    }



#' @rdname updateObject
#' @export
setMethod(
    f = "updateObject",
    signature = signature(object = "DESeqAnalysis"),
    definition = `updateObject,DESeqAnalysis`
)
