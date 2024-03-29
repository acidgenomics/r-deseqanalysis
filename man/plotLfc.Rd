% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllGenerics.R, R/plotLfc-methods.R
\name{plotLfc}
\alias{plotLfc}
\alias{plotLfc,DESeqAnalysis-method}
\title{Plot log2 fold change distributions}
\usage{
plotLfc(object, ...)

\S4method{plotLfc}{DESeqAnalysis}(object)
}
\arguments{
\item{object}{Object.}

\item{...}{Additional arguments.}
}
\value{
\code{ggplot}.
}
\description{
Plot log2 fold change distributions
}
\note{
Updated 2022-05-17.

Plot the log2 fold change distributions for all contrasts in the analysis.
}
\examples{
data(deseq)

## DESeqAnalysis ====
object <- deseq
lfcThreshold(object) <- 0.5
plotLfc(object)
}
