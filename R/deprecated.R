## nocov start
## nolint start



#' @name deprecated
#' @inherit AcidRoxygen::deprecated description examples return seealso title
#' @inheritParams AcidRoxygen::params
#' @keywords internal
NULL



## v0.2.12 =====================================================================
#' @rdname deprecated
#' @usage NULL
#' @export
apeglmContrast <- function(dds, ...) {
    .Deprecated("apeglmResults")
    apeglmResults(object = dds, ...)
}



## v0.4.0 ======================================================================
## Soft deprecated, since this is used in bcbioRNASeq F1000 paper.
#' @rdname plotMA
#' @usage NULL
#' @export
plotMeanAverage <- function(...) {
    ## > .Deprecated("plotMA")
    plotMA(...)
}



## nolint end
## nocov end
