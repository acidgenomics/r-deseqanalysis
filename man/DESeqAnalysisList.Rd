% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllGenerics.R, R/DESeqAnalysisList-methods.R
\name{DESeqAnalysisList}
\alias{DESeqAnalysisList}
\alias{DESeqAnalysisList,SimpleList-method}
\alias{DESeqAnalysisList,list-method}
\alias{DESeqAnalysisList,missing-method}
\title{List containing related DESeq2 analyses}
\usage{
DESeqAnalysisList(object, ...)

\S4method{DESeqAnalysisList}{SimpleList}(object)

\S4method{DESeqAnalysisList}{list}(object)

\S4method{DESeqAnalysisList}{missing}(object)
}
\arguments{
\item{object}{Object.}

\item{...}{Additional arguments.}
}
\value{
\code{DESeqAnalysisList}.
}
\description{
List containing related DESeq2 analyses
}
\note{
Updated 2021-03-15.
}
\examples{
data(deseq)
x <- DESeqAnalysisList(list("object1" = deseq, "object2" = deseq))
x
}
