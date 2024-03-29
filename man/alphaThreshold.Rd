% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllGenerics.R, R/alphaThreshold-methods.R
\name{alphaThreshold}
\alias{alphaThreshold}
\alias{alphaThreshold<-}
\alias{alphaThreshold,DESeqAnalysis-method}
\alias{alphaThreshold,DESeqAnalysisList-method}
\alias{alphaThreshold,DESeqResults-method}
\alias{alphaThreshold,DESeqResultsList-method}
\alias{alphaThreshold<-,DESeqAnalysis,numeric-method}
\alias{alphaThreshold<-,DESeqAnalysis,NULL-method}
\alias{alphaThreshold<-,DESeqAnalysisList,numeric-method}
\alias{alphaThreshold<-,DESeqAnalysisList,NULL-method}
\alias{alphaThreshold<-,DESeqResults,numeric-method}
\alias{alphaThreshold<-,DESeqResultsList,numeric-method}
\alias{alphaThreshold<-,DESeqResultsList,NULL-method}
\title{Alpha threshold}
\usage{
alphaThreshold(object, ...)
alphaThreshold(object, ...) <- value

\S4method{alphaThreshold}{DESeqAnalysis}(object)

\S4method{alphaThreshold}{DESeqAnalysisList}(object)

\S4method{alphaThreshold}{DESeqResults}(object)

\S4method{alphaThreshold}{DESeqResultsList}(object)

\S4method{alphaThreshold}{DESeqAnalysis,numeric}(object) <- value

\S4method{alphaThreshold}{DESeqAnalysis,`NULL`}(object) <- value

\S4method{alphaThreshold}{DESeqAnalysisList,numeric}(object) <- value

\S4method{alphaThreshold}{DESeqAnalysisList,`NULL`}(object) <- value

\S4method{alphaThreshold}{DESeqResults,numeric}(object) <- value

\S4method{alphaThreshold}{DESeqResultsList,numeric}(object) <- value

\S4method{alphaThreshold}{DESeqResultsList,`NULL`}(object) <- value
}
\arguments{
\item{object}{Object.}

\item{value}{Value to assign.}

\item{...}{Additional arguments.}
}
\value{
\code{numeric(1)}.
Alpha cutoff level.
}
\description{
Alpha threshold
}
\details{
Assumes \code{0.01} by default if unset.
}
\note{
Updated 2021-03-10.
}
\examples{
data(deseq)

## DESeqAnalysis ====
object <- deseq
alphaThreshold(object) <- 0.05
alphaThreshold(object)
}
