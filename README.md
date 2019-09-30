# DESeqAnalysis

[![Repo status: active](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Travis CI build status](https://travis-ci.com/acidgenomics/DESeqAnalysis.svg?branch=master)](https://travis-ci.com/acidgenomics/DESeqAnalysis)
[![AppVeyor CI build status](https://ci.appveyor.com/api/projects/status/s5evl37t3vjkfjct?svg=true)](https://ci.appveyor.com/project/mjsteinbaugh/deseqanalysis)
[![Anaconda version](https://anaconda.org/bioconda/r-deseqanalysis/badges/version.svg) ![Anaconda latest release date](https://anaconda.org/bioconda/r-deseqanalysis/badges/latest_release_date.svg) ![Anaconda downloads](https://anaconda.org/bioconda/r-deseqanalysis/badges/downloads.svg)](https://anaconda.org/bioconda/r-deseqanalysis)

Toolkit for performing differential expression with [DESeq2][].

## Installation

### [R][] method

```r
if (!requireNamespace("remotes", quietly = TRUE)) {
    install.packages("remotes")
}
Sys.setenv(R_REMOTES_UPGRADE = "always")
# Set `GITHUB_PAT` in `~/.Renviron` if you get a rate limit error.
remotes::install_github("acidgenomics/DESeqAnalysis")
```

### [conda][] method

Configure [conda][] to use the [bioconda][] channels.

```sh
conda install -c bioconda r-deseqanalysis
```

[BiocManager]: https://cran.r-project.org/package=BiocManager
[Bioconductor]: https://bioconductor.org/
[DESeq2]: https://bioconductor.org/packages/DESeq2/
[R]: https://www.r-project.org/
[bioconda]: https://bioconda.github.io/
[conda]: https://conda.io/
