## nocov start
## nolint start



#' @name deprecated
#' @inherit acidroxygen::deprecated description examples return seealso title
#' @inheritParams acidroxygen::params
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



## nolint end
## nocov end
