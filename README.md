# DESeqAnalysis

[![Repo status: active](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Travis CI build status](https://travis-ci.com/acidgenomics/DESeqAnalysis.svg?branch=master)](https://travis-ci.com/acidgenomics/DESeqAnalysis)
[![AppVeyor CI build status](https://ci.appveyor.com/api/projects/status/s5evl37t3vjkfjct?svg=true)](https://ci.appveyor.com/project/mjsteinbaugh/deseqanalysis)
[![Install with Bioconda](https://img.shields.io/badge/install%20with-bioconda-brightgreen.svg?style=flat)](http://bioconda.github.io/recipes/r-deseqanalysis/README.html)

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

### [Conda][] method

Configure [Conda][] to use the [Bioconda][] channels.

```sh
# Don't install recipe into base environment.
name="r-deseqanalysis"
conda create --name="$name" "$name"
conda activate "$name"
R
```

### [Docker][] method

```sh
image="acidgenomics/r-rnaseq"
workdir="/mnt/work"
docker pull "$image"
docker run -it \
    --volume="${PWD}:${workdir}" \
    --workdir="$workdir" \
    "$image" \
    R
```

[biocmanager]: https://cran.r-project.org/package=BiocManager
[bioconda]: https://bioconda.github.io/
[bioconductor]: https://bioconductor.org/
[conda]: https://conda.io/
[deseq2]: https://bioconductor.org/packages/DESeq2/
[docker]: https://www.docker.com/
[r]: https://www.r-project.org/
