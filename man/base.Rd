% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/base-methods.R
\name{base}
\alias{base}
\alias{colnames,DESeqAnalysis-method}
\alias{dim,DESeqAnalysis-method}
\alias{dimnames,DESeqAnalysis-method}
\alias{names,DESeqAnalysis-method}
\alias{rownames,DESeqAnalysis-method}
\title{Base methods}
\usage{
\S4method{colnames}{DESeqAnalysis}(x, do.NULL = TRUE, prefix = "col")

\S4method{dim}{DESeqAnalysis}(x)

\S4method{dimnames}{DESeqAnalysis}(x)

\S4method{names}{DESeqAnalysis}(x)

\S4method{rownames}{DESeqAnalysis}(x, do.NULL = TRUE, prefix = "row")
}
\arguments{
\item{x}{Object.}

\item{do.NULL}{logical. If \code{FALSE} and names are \code{NULL}, names are created.}

\item{prefix}{for created names.}
}
\value{
Varies, dependending on function.
}
\description{
Base methods
}
\note{
Updated 2022-05-17.
}
\examples{
data(deseq)

head(colnames(deseq))
head(rownames(deseq))

head(names(deseq))

lapply(X = dimnames(deseq), FUN = head)
}
\keyword{internal}
