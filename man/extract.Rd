% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract-methods.R
\name{extract}
\alias{extract}
\alias{[,DESeqAnalysis,ANY,ANY,ANY-method}
\title{Extract or replace parts of an object}
\usage{
\S4method{[}{DESeqAnalysis,ANY,ANY,ANY}(x, i, j, drop = FALSE)
}
\arguments{
\item{x}{
    object from which to extract element(s) or in which to replace element(s).
  }

\item{i}{
    indices specifying elements to extract or replace.  Indices are
    \code{numeric} or \code{character} vectors or empty (missing) or
    \code{NULL}.  Numeric values are coerced to integer as by
    \code{\link[base]{as.integer}} (and hence truncated towards zero).
    Character vectors will be matched to the \code{\link[base]{names}} of the
    object (or for matrices/arrays, the \code{\link[base]{dimnames}}):
    see \sQuote{Character indices} below for further details.

    For \code{[}-indexing only: \code{i}, \code{j}, \code{\dots} can be
    logical vectors, indicating elements/slices to select.  Such vectors
    are recycled if necessary to match the corresponding extent.
    \code{i}, \code{j}, \code{\dots} can also be negative integers,
    indicating elements/slices to leave out of the selection.

    When indexing arrays by \code{[} a single argument \code{i} can be a
    matrix with as many columns as there are dimensions of \code{x}; the
    result is then a vector with elements corresponding to the sets of
    indices in each row of \code{i}.

    An index value of \code{NULL} is treated as if it were \code{integer(0)}.
  }

\item{j}{
    indices specifying elements to extract or replace.  Indices are
    \code{numeric} or \code{character} vectors or empty (missing) or
    \code{NULL}.  Numeric values are coerced to integer as by
    \code{\link[base]{as.integer}} (and hence truncated towards zero).
    Character vectors will be matched to the \code{\link[base]{names}} of the
    object (or for matrices/arrays, the \code{\link[base]{dimnames}}):
    see \sQuote{Character indices} below for further details.

    For \code{[}-indexing only: \code{i}, \code{j}, \code{\dots} can be
    logical vectors, indicating elements/slices to select.  Such vectors
    are recycled if necessary to match the corresponding extent.
    \code{i}, \code{j}, \code{\dots} can also be negative integers,
    indicating elements/slices to leave out of the selection.

    When indexing arrays by \code{[} a single argument \code{i} can be a
    matrix with as many columns as there are dimensions of \code{x}; the
    result is then a vector with elements corresponding to the sets of
    indices in each row of \code{i}.

    An index value of \code{NULL} is treated as if it were \code{integer(0)}.
  }

\item{drop}{For matrices and arrays.  If \code{TRUE} the result is
    coerced to the lowest possible dimension (see the examples).  This
    only works for extracting elements, not for the replacement.  See
    \code{\link[base]{drop}} for further details.
  }
}
\value{
\code{DESeqAnalysis}.
}
\description{
Extract genes by row and samples by column.
}
\note{
Updated 2020-10-28.
}
\examples{
data(deseq)
object <- deseq
dim(object)

genes <- head(rownames(object), 50L)
head(genes)
samples <- head(colnames(object), 2L)
head(samples)

x <- object[genes, samples]
print(x)
}
\references{
Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
