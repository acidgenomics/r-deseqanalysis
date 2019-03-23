# DESeqAnalysis

[![Travis CI](https://travis-ci.com/acidgenomics/DESeqAnalysis.svg?branch=master)](https://travis-ci.com/acidgenomics/DESeqAnalysis)
[![AppVeyor CI](https://ci.appveyor.com/api/projects/status/s5evl37t3vjkfjct?svg=true)](https://ci.appveyor.com/project/mjsteinbaugh/deseqanalysis)
[![Codecov](https://codecov.io/gh/acidgenomics/DESeqAnalysis/branch/master/graph/badge.svg)](https://codecov.io/gh/acidgenomics/DESeqAnalysis)
[![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)

Toolkit for performing differential expression with [DESeq2][].

## Installation

This is an [R][] package.

### [Bioconductor][] method

We recommend installing the package with [BiocManager][].

```r
if (!require("BiocManager")) {
    install.packages("BiocManager")
}
BiocManager::install("remotes")
BiocManager::install("acidgenomics/DESeqAnalysis")
```

[BiocManager]: https://cran.r-project.org/package=BiocManager
[Bioconductor]: https://bioconductor.org/
[DESeq2]: https://bioconductor.org/packages/DESeq2/
[R]: https://www.r-project.org/
