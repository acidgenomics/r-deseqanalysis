% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllGenerics.R, R/results-methods.R
\name{results}
\alias{results}
\alias{results,DESeqAnalysis-method}
\alias{results,DESeqDataSet-method}
\title{Results}
\usage{
results(object, ...)

\S4method{results}{DESeqAnalysis}(object, i, lfcShrink = NULL, extra = FALSE, quiet = TRUE)

\S4method{results}{DESeqDataSet}(object, ...)
}
\arguments{
\item{object}{Object.}

\item{i}{Indices specifying elements to extract or replace. Indices are \code{numeric} or
\code{character} vectors, empty (\code{missing}), or \code{NULL}.

For more information:

\if{html}{\out{<div class="sourceCode">}}\preformatted{help(topic = "Extract", package = "base")
}\if{html}{\out{</div>}}}

\item{lfcShrink}{\code{logical(1)} or \code{NULL}.
Use shrunken log2 fold change (LFC) values.
If \code{NULL}, inherits value defined in \code{lfcShrink()}.}

\item{extra}{\code{logical(1)}.
Include row data (i.e. gene metadata) and normalized counts from the
internal \code{DESeqDataSet}.}

\item{quiet}{\code{logical(1)}.
Perform command quietly, suppressing messages.}

\item{...}{Additional arguments.}
}
\value{
\code{DESeqResults}.
}
\description{
Results
}
\section{Functions}{
\itemize{
\item \code{results(DESeqDataSet)}: Arguments pass through to \code{DESeq2::results()}.

}}
\note{
Updated 2022-08-30.
}
\section{Extra mode}{


Get the \code{DESeqDataSet}, and humanize the sample names. Note that we're not
calling \code{humanize()} here on the \code{DESeqDataSet}, because we want to keep the
gene identifiers in the row names. Use human-friendly sample names, defined
by the \code{sampleName} column in \code{colData}. We're using this downstream when
joining the normalized counts.
}

\examples{
data(deseq)

## DESeqAnalysis ====
x <- results(deseq, i = 1L)
class(x)
colnames(x)
}
