% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllGenerics.R, R/resultsMatrix-methods.R
\name{resultsMatrix}
\alias{resultsMatrix}
\alias{resultsMatrix,DESeqAnalysis-method}
\alias{resultsMatrix,DESeqAnalysisList-method}
\title{DESeq aggregate results matrix}
\usage{
resultsMatrix(object, ...)

\S4method{resultsMatrix}{DESeqAnalysis}(
  object,
  value = c("log2FoldChange", "stat", "padj"),
  rowData = FALSE
)

\S4method{resultsMatrix}{DESeqAnalysisList}(
  object,
  value = c("log2FoldChange", "stat", "padj"),
  rowData = FALSE
)
}
\arguments{
\item{object}{\code{DESeqAnalysis}.}

\item{value}{\code{character(1)}.
Value type to return. Corresponds to supported \code{DESeqResults} column:
\itemize{
\item \code{log2FoldChange}: log2 fold change.\cr
This will return \emph{shrunken} LFC values if they are defined.
\item \code{stat}: Wald test statistic.
\item \code{padj}: BH adjusted \emph{P} value.
}}

\item{rowData}{\code{logical(1)}.
Include row (gene) annotations, bound to the left side of the data frame.}

\item{...}{Additional arguments.}
}
\value{
\itemize{
\item \code{rowData = FALSE}: \code{matrix}.
\item \code{rowData = TRUE}: \code{DataFrame}.
}
}
\description{
Generate an aggregate matrix of \code{DESeqResults} column values per contrast.
}
\section{Functions}{
\itemize{
\item \code{resultsMatrix,DESeqAnalysisList-method}: Loop across the nested \code{DESeqAnalysis} objects and
aggregate the corresponding result matrices. Note that the analysis names
are automatically prefixed to the column names.
}}

\note{
Updated 2021-03-15.
}
\examples{
data(deseq)

## DESeqAnalysis ====
x <- resultsMatrix(deseq)
head(x)
}
