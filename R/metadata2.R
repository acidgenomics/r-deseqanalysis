## FIXME Move this to transformer.



#' Metadata
#'
#' Dynamically handles metadata assignment and extraction in a similar fashion
#' for both S3 and S4 objects.
#'
#' @name metadata2
#' @note Updated 2019-10-24.
#'
#' @section S3 class:
#'
#' Internally slots into [`attributes()`][base::attributes].
#' Attribute is also accessible via `attr(object, which)`.
#'
#' @section S4 class:
#'
#' Requires that object extends [`Annotated`][S4Vectors::Annotated-class] class,
#' which supports [`metadata()`][S4Vectors::metadata].
#'
#' Internally slots into [`metadata()`][S4Vectors::metadata].
#' Attribute is also accessible via `metadata(object)[[which]]`.
#'
#' @inheritParams acidroxygen::params
#' @param which `character(1)`.
#'   A non-empty character string specifying which attribute is to be accessed.
#' @param value `ANY`.
#'   Metadata values to assign into slot, defined by `which`.
#'
#' @return
#' - `metadata2()`: Metadata.
#' - `metadata2<-()`: Modified object.
NULL



#' @rdname metadata2
#' @export
setGeneric(
    name = "metadata2",
    def = function(x, which, ...) {
        standardGeneric("metadata2")
    }
)



#' @rdname metadata2
#' @export
setGeneric(
    name = "metadata2<-",
    def = function(x, which, ..., value) {
        standardGeneric("metadata2<-")
    }
)



## Intended for S3 class, or S4 class that doesn't support `metadata()`.
## Updated 2019-10-24.
`metadata2,ANY` <-  # nolint
    function(x, which) {
        assert(
            isString(which),
            isSubset(which, names(attributes(x)))
        )
        attr(x = x, which = which, exact = TRUE)
    }



#' @rdname metadata2
#' @export
setMethod(
    f = "metadata2",
    signature = signature("ANY"),
    definition = `metadata2,ANY`
)



## Intended for S4 class supporting `metadata()`.
## Updated 2019-10-24.
`metadata2,Annotated` <-  # nolint
    function(x, which) {
        assert(
            isString(which),
            isSubset(which, names(metadata(x)))
        )
        metadata(x)[[which]]
    }



#' @rdname metadata2
#' @export
setMethod(
    f = "metadata2",
    signature = signature("Annotated"),
    definition = `metadata2,Annotated`
)



## Intended for S3 class, or S4 class that doesn't support `metadata()`.
## Updated 2019-10-24.
`metadata2<-,ANY,ANY` <-  # nolint
    function(x, which, value) {
        attr(x = x, which = which) <- value
        x
    }



#' @rdname metadata2
#' @export
setReplaceMethod(
    f = "metadata2",
    signature = signature(
        x = "ANY",
        which = "character",
        value = "ANY"
    ),
    definition = `metadata2<-,ANY,ANY`
)



## Intended for S3 class, or S4 class that doesn't support `metadata()`.
## Updated 2019-10-24.
`metadata2<-,Annotated,ANY` <-  # nolint
    function(x, which, value) {
        metadata(x)[[which]] <- value
        x
    }



#' @rdname metadata2
#' @export
setReplaceMethod(
    f = "metadata2",
    signature = signature(
        x = "Annotated",
        which = "character",
        value = "ANY"
    ),
    definition = `metadata2<-,Annotated,ANY`
)
