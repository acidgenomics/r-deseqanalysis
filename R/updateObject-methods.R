#' @name updateObject
#' @inherit BiocGenerics::updateObject
#' @note Updated 2020-08-04.
#'
#' @section `DESeqAnalysis`:
#'
#' In the v0.1.8 update, we improved the class to contain `Annotated` virtual
#' class, which adds support for the `metadata()` slot. Objects saved by older
#' package versions must be updated to account for this change.
#'
#' @return `DESeqAnalysis`.
#'
#' @examples
#' data(deseq)
#'
#' ## DESeqAnalysis ====
#' updateObject(deseq)
NULL



## Updated 2020-08-04.
`updateObject,DESeqAnalysis` <-  # nolint
    function(object, ..., verbose = FALSE) {
        assert(isFlag(verbose))
        if (!isTRUE(.hasSlot(object, "metadata"))) {
            if (isTRUE(verbose)) {
                ## nocov start
                message(
                    "Legacy object < 0.1.8 detected.\n",
                    "Updating to support metadata slot."
                )
                ## nocov end
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
    signature = signature("DESeqAnalysis"),
    definition = `updateObject,DESeqAnalysis`
)
