# DESeqAnalysis

![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-green.svg)
[![Install with Bioconda](https://img.shields.io/badge/install%20with-bioconda-brightgreen.svg?style=flat)](http://bioconda.github.io/recipes/r-deseqanalysis/README.html)

Toolkit for performing differential expression with [DESeq2][].

## Installation

### [R][] method

```r
if (!requireNamespace("BiocManager", quietly = TRUE)) {
    install.packages("BiocManager")
}
install.packages(
    pkgs = "DESeqAnalysis",
    repos = c(
        "https://r.acidgenomics.com",
        BiocManager::repositories()
    ),
    dependencies = TRUE
)
```

### [Conda][] method

Configure [Conda][] to use the [Bioconda][] channels.

```sh
# Don't install recipe into base environment.
name='r-deseqanalysis'
conda create --name="$name" "$name"
conda activate "$name"
R
```

[bioconda]: https://bioconda.github.io/
[conda]: https://conda.io/
[deseq2]: https://bioconductor.org/packages/DESeq2/
[r]: https://www.r-project.org/
