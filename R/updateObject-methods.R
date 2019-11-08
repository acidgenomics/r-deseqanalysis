#' @name updateObject
#' @inherit BiocGenerics::updateObject
#' @note Updated 2019-08-20.
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
#' updateObject(deseq)
NULL



## Updated 2019-07-25.
`updateObject,DESeqAnalysis` <-  # nolint
    function(object) {
        if (!isTRUE(.hasSlot(object, "metadata"))) {
            ## nocov start
            message(
                "Legacy object < 0.1.8 detected.\n",
                "Updating to support metadata slot."
            )
            ## nocov end
        }
        data <- object@data
        transform <- object@transform
        results <- object@results
        lfcShrink <- object@lfcShrink
        DESeqAnalysis(
            data = data,
            transform = transform,
            results = results,
            lfcShrink = lfcShrink
        )
    }



#' @rdname updateObject
#' @export
setMethod(
    f = "updateObject",
    signature = signature("DESeqAnalysis"),
    definition = `updateObject,DESeqAnalysis`
)
