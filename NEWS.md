## DESeqAnalysis 0.3.4 (2020-08-18)

### New functions

- `degIntersection`: Utility function that will return the number of times a
  DEG is detected across contrasts.

## DESeqAnalysis 0.3.3 (2020-08-17)

### Minor changes

- `apeglmResults`: Simplified default arguments and disabled `...` passthrough
  to `lfcShrink`, which is a bit confusing. Note that `DESeq` and `lfcShrink`
  are now called internally with `parallel = TRUE`.

## DESeqAnalysis 0.3.2 (2020-08-13)

### Minor changes

- Added `i` argument support to `degPerContrast`, and the plotting functions
  `plotDEGUpset` and `plotStackedBar`, which call `degPerContrast` internally.
  This allows the user to pick specific contrasts of interest to plot.
- `plotDEGStackedBar`: Now allowing the user to pick only up- or down-regulated
  genes, which can be useful.
- Bug fix for `apeglmResults` cli display code.

## DESeqAnalysis 0.3.1 (2020-08-07)

### Minor changes

- Adjusted minimum dependency versions so package will build on bioconda.

## DESeqAnalysis 0.3.0 (2020-08-05)

### Major changes

- `DESeqAnalysis` object now supports stashing of threshold metadata:
    - `alphaThreshold`.
    - `lfcShrink`: whether to use (adaptie) shrunken LFC values or not.
    - `lfcThreshold` (for post-hoc cutoffs).
    - `baseMeanThreshold`.
  These are defined in the `metadata` slot of the object and can be accessed/
  modified with corresponding S4 generics.
- Updated default differential expression R Markdown template to support
  additional parameters:
    - alpha_threshold
    - base_mean_threshold
    - contrast_samples
    - lfc_shrink
    - lfc_threshold

### Minor changes

- Switched to cli package for more informative messages.

## DESeqAnalysis 0.2.21 (2020-07-29)

### Major changes

- Added support for `baseMeanThreshold`, where applicable. Applies primarily
  to `deg`, `plotDEGHeatmap`, `plotDEGPCA`, `plotMA`, and `plotVolcano`,
  and `resultsTables`.
- Reworked internal code that handles DEG extraction. Note that `lfcThreshold`
  and `baseMeanThreshold` now use `>=` logic rather than previous `>` approach.
  The alpha cutoff still uses `>` operator logic.

### Minor changes

- Removed UpSetR dependency, in preparation for future switch to ggupset.

## DESeqAnalysis 0.2.20 (2020-05-12)

### Minor changes

- Split out `setValidity` call separately from `setClass`.
- `updateObject`: `FGSEAList` method now uses `...` and `verbose` arguments,
  as defined in the S4 generic.

## DESeqAnalysis 0.2.19 (2020-03-16)

### Minor changes

- Resaved example test data set, using Ensembl 99 annotations.
- Updated documentation using roxygen2 v7.1.

## DESeqAnalysis 0.2.18 (2020-01-20)

### Minor changes

- Updated basejump dependencies. Namely, bioverbs package has been renamed to
  acidgenerics, prior to CRAN submission.

## DESeqAnalysis 0.2.17 (2020-01-03)

### New functions

- `combine`: Method support for combining multiple results contrasts into a
  single `DESeqAnalysis` object. Note that the internal `DESeqDataSet` and
  `DESeqTransform` objects defined in `x` and `y` must match.

## DESeqAnalysis 0.2.16 (2019-12-18)

### New functions

- `resultsDiff`: Compare the effect difference between two `DESeqResults`
  contrasts. Useful for evaluating the relative effect of compounds, RNAi
  clones, CRISPR guides, for example.

### Minor changes

- `contrastSamples`: Added support for interaction effect (difference of
  differences) contrasts. For example: "group_B_vs_A_group_C_vs_A_effect".
- `plotCounts`: Added `samples` argument support for `DESeqAnalysis` method.
  This is useful for quick sample selection when generating plots.
- Improved internal consistency of `lfcShrink` handling in `results` calls.
  Note that `reuslts` still defaults to `lfcShrink = FALSE`, whereas other
  plotting and results table functions default to `lfcShrink = TRUE`.
- `deg`: Added alpha and LFC information in message.

## DESeqAnalysis 0.2.15 (2019-12-13)

### Minor changes

- Now allowing the user to override internal `alpha` and `lfcThreshold` for
  these plots: `plotDEGHeatmap`, `plotDEGPCA`, `plotDEGUpset`, `plotMA`,
  `plotVolcano`.

## DESeqAnalysis 0.2.14 (2019-12-04)

### Minor changes

- `deg`: Fixed `DESeqAnalysis` method passthrough to `DESeqResults`, allowing
  use of `alpha` and `lfcThreshold`, for post-hoc cutoffs.

## DESeqAnalysis 0.2.13 (2019-11-19)

### Minor changes

- Deprecated `apeglmContrast` in favor of modified S4 method approach now
  defined in `apeglmResults`. The internal code is otherwise unmodified.
- `plotDEGHeatmap`: Simplified argument handling and internal passthrough to
  `plotHeatmap` `SummarizedExperiment` method defined in acidplots.

## DESeqAnalysis 0.2.12 (2019-11-18)

### Minor changes

- `plotCounts`: Added method support for `DESeqTransform`. Also added a
  `transform` argument to `DESeqAnalysis` method, which allows the user to
  easily plot variance-stabilized counts using the slotted `DESeqTransform`
  object.

## DESeqAnalysis 0.2.11 (2019-11-12)

### Major changes

- Renamed `results` argument to `i` for `DESeqAnalysis` methods, where
  applicable. This improves consistency with Bioconductor conventions (similar
  to `assay` method, which also uses `i` for position). Note that previous
  usage of `results` is now defunct and should intentionally error.

### Minor changes

- `resultsMatrix`: Added support for `baseMean` aggregation, and improved
  documentation.
- Updated basejump dependency versions.
- Added tximport as a suggested package.
- Updated dependencies to require Bioconductor 3.10 release.
- Added `plotHeatmap`, `plotCorrelationHeatmap`, and `plotQuantileHeatmap`
  method support, which extracts `DESeqTransform` object internally and hands
  off to `SummarizedExperiment` methods defined in [acidplots][].

## DESeqAnalysis 0.2.10 (2019-10-16)

### Minor changes

- `plotDEGHeatmap`, `plotDEGPCA`: Hardened methods against input with very few
  DEGs (less than 10). Simplified internal method passthrough to no longer
  rely upon `matchArgsToDoCall` function.
- `plotBaseMean`: Now exporting `numeric` method, which is the basis for
  `DESeqDataSet` and `DESeqResults` methods.

## DESeqAnalysis 0.2.9 (2019-10-09)

### New functions

- `matchMetadataFiles`: Quickly generate a mapping data frame of user input
  metadata sample names to the expected tximport quant directory names
  (e.g. salmon, kallisto output). This is useful when a user provides metadata
  that doesn't match the FASTQ names exactly.
- `prepareTximportFiles`: Automatically assigns valid sample names to tximport
  quant file import. Tested primarily against salmon and kallisto files.

## DESeqAnalysis 0.2.8 (2019-09-17)

### New functions

- `apeglmContrast`: New utility function that enables `lfcShrink` using apeglm
  without having to mentally deal with `coef` argument.

### Major changes

- Now attached DESeq2 automatically via "Depends" in `DESCRIPTION`.

### Minor changes

- `plotBaseMean`: Added argument support to define colors and disable summary
  statistics lines. Simplified internal passthrough in `DESeqAnalysis`,
  `DESeqDataSet`, and `DESeqResults` methods.

## DESeqAnalysis 0.2.7 (2019-09-11)

### Minor changes

- Bug fixes for contrast name handling with user-defined resultsNames.
  `DESeqAnalysis` methods now stash contrast name into `metadata` as
  `contrastName` for `DESeqResults` objects, which allows for easier contrast
  name handling inside plotting and other markdown functions. We needed to add
  a new `useStash` argument for `contrastName` that disables this behavior, so
  `contrastSamples` can pull the samples from a pairwise contrast easily.

## DESeqAnalysis 0.2.6 (2019-09-09)

### New functions

- `importPairwiseContrasts`: Quickly import pairwise contrasts definining
  numerator and denominator for a specified group.

## DESeqAnalysis 0.2.5 (2019-08-27)

### Minor changes

- Requiring R 3.6. Updated basejump and Bioconductor dependencies.
- Improved consistency of `DataFrame` usage inside `plotMA` and `plotVolcano`,
  using our `leftJoin` method internally.

## DESeqAnalysis 0.2.4 (2019-08-20)

### Minor changes

- Removed dplyr dependencies in `plotMA` and `plotVolcano`. Reworked internal
  code using base R methods against `DataFrame` class instead of `tbl_df`.
- Split out documentation on S4 generator functions into separate Rd files.
- Updated basejump dependecy versions.
- `topTables`: Now using `DataFrame`-oriented internal code to return the
  `kable` output, rather than relying upon `tbl_df` with dplyr.

## DESeqAnalysis 0.2.3 (2019-08-06)

### Minor changes

- Improved documentation consistency by using shared roxygen params from new
  acidroxygen package.
- Updated basejump dependency versions.

## DESeqAnalysis 0.2.2 (2019-08-02)

### New functions

- `plotBaseMean`: Quickly visualize the count distribution across all samples.

### Minor changes

- Improved Dockerized Travis CI configuration, using new Rcheck repo.

## DESeqAnalysis 0.2.1 (2019-07-30)

### Minor changes

- Updated basejump dependencies.
- Improved code coverage and documentation.
- Updated example dataset, based on latest acidtest RangedSummarizedExperiment.

## DESeqAnalysis 0.2.0 (2019-07-23)

Version bump, reflecting change in basejump dependency.

## DESeqAnalysis 0.1.22 (2019-04-28)

### New functions

- `plotPCA`: Added improved method support for `DESeqTransform`, masking the
  method exported in DESeq2. Also added corresponding method support for
  `DESeqAnalysis` class, which uses `DESeqTransform` method internally.

### Minor changes

- Removed `plotMA2` export, which keeps DESeq2 `plotMA` method as an alias.

## DESeqAnalysis 0.1.21 (2019-04-26)

### New functions

- `plotDEGUpset`: Support for easily plotting bidirectional (up/down) DEG
  intersections across contrasts, using the UpSetR package.

### Minor changes

- Improved `show` method to include `alpha` and `lfcThreshold` information.
- `topTables`: Added `lfcShrink` support.

## DESeqAnalysis 0.1.20 (2019-04-25)

### New functions

- Now exporting `lfcShrinkType`, and `transformType`, which were previously used
  internally but are generally useful.

### Major changes

- `plotDEGHeatmap`: Improved default color breaks. Now defaults to
  blue-black-yellow default color scheme (see `acidplots::blueYellow`).
- Removed internal `.matchResults` function in favor of consistently using
  `results` instead throughout the package.

### Minor changes

- Slotting `lfcShrink` into object is now optional again.
- Added `lfcShrink` argument support to `plotDEGHeatmap`, `topTables`, and
  `export`.
- `plotMA`, `plotVolcano`: Now suppressing duplicate gene message returned by
  internal `Gene2Symbol` call, which isn't informative here.
- Added `lfcThreshold` and `lfcShrink` information on plots, where applicable.

## DESeqAnalysis 0.1.19 (2019-04-23)

### Minor changes

- Converted `resultsMatrix` to an S4 generic and added initial support for
  `DESeqAnalysis` and `DESeqAnalysisList` class objects.

## DESeqAnalysis 0.1.18 (2019-04-23)

### Minor changes

- NAMESPACE and documentation updates, incorporating new [acidplots][] package.

## DESeqAnalysis 0.1.17 (2019-04-08)

Bug fixes to improve handling of `DESeqDataSet` generated using RefSeq gene
annotations.

### Minor changes

- `contrastNames`: Improved internal regular expression matching to properly
  detect a contrast factor prefix that contains an underscore. This now works
  by checking against the column names of `colData` internally.
- `topTables`: `geneID`, `geneName`, and `geneBiotype` are now optional columns.
- Updated Travis CI configuration to use rnaseq Docker image.

## DESeqAnalysis 0.1.16 (2019-04-08)

### Major changes

- `export`: Removed `humanize` argument.
- `plotCounts`: Removed `transform` argument.
- `plotDEGHeatmap`, `plotDEGPCA`: Removed `counts` argument.
- `plotMA`, `plotVolcano`: Improved color support and gene-to-symbol handling.

### Minor changes

- `topTables`: Simplifed internal method code for `DESeqAnalysis` class.
- Consistently importing British spelling variants for ggplot2 functions.
- Updated unit tests for plotting functions.

## DESeqAnalysis 0.1.15 (2019-04-03)

### New functions

- `resultsMatrix`: Utility function that quickly aggregates `DESeqResults`
  values for multiple contrasts into a single `matrix`. Currently supports
  return of `log2FoldChange`, `stat`, or `padj` values.

### Minor changes

- Reorganized unit tests and improved code coverage.

## DESeqAnalysis 0.1.14 (2019-03-29)

### Minor changes

- Compatibility update for basejump v0.10 release. Some plotting code
  dependencies have been split out to firestarter and minimalism packages.
  We have updated the internal code dependencies to reflect these changes.
  This includes `plotCounts`, `plotDEGHeatmap` (i.e. `plotHeatmap`), and
  `plotDEGPCA` (i.e. `plotPCA`).
- Updated documentation on global formals, which now use `acid` prefix instead
  of `basejump`.

## DESeqAnalysis 0.1.13 (2019-03-23)

### Minor changes

- Migrated code to [Acid Genomics][].

## DESeqAnalysis 0.1.12 (2019-03-18)

### Minor changes

- Updated differential expression template and dependencies.

## DESeqAnalysis 0.1.11 (2019-03-11)

### Minor changes

- `plotCounts`: Removed `DESeqDataSet` method support, which is now covered
  inside `SummarizedExperiment` method support in [basejump][].
- Updated differential expression R Markdown template to improve
  parameterization support.

## DESeqAnalysis 0.1.10 (2019-02-18)

### Major changes

- `plotDEGHeatmap`, `plotDEGPCA`: Reworked internal code to export
  `DESeqResults` method that maintains backward compatibility with approach
  used in bcbioRNASeq F1000 paper.
- Exported additional legacy `DESeqResults` method support for `plotMA`,
  `plotVolcano`, `resultsTables`, and `topTables`, to maintain backward
  compatibility with bcbioRNASeq R package.

### Minor changes

- Updated NEWS to include information on all previous releases.

## DESeqAnalysis 0.1.9 (2019-02-12)

### Minor changes

- R 3.4 compatibility fix for `resultsTables`, which needs to assign rownames
  on `rowData` return.

## DESeqAnalysis 0.1.8 (2019-02-08)

This release reworks the S4 class structure and adds `DESeqAnalysisList`. Note
that objects previously saved prior to the v0.1.8 release will no longer return
valid when checked using `validObject`. They can be updated using
`updateObject`.

### New classes

- `DESeqAnalysisList`: extends `SimpleList`, and enables saving of multiple
  analyses (e.g. per patient clinical data) in a single object. This class makes
  it much easier to report on multiple complex contrasts.

### Major changes

- `DESeqAnalysis` now inherits from `Annotated` class. This enables support for
  `metadata` slot, which can be used to store package version.
- Improved checks in `DESeqAnalysis` validity method.

### Minor changes

- Added support for `plotCounts` generic, in favor of now deprecated `plotGene`.
- Added support for `results` and `resultsNames` generics.
- Added internal code to check `lfcShrink` type (e.g. apeglm).
- Added `sampleData` method support for `DESeqAnalysis`, thereby simplying
  the `sample_data` chunk in differential expression R Markdown template.
- Updated and improved `show` method for `DESeqAnalysis`. This now will return
  the version of DESeqAnalysis and not just DESeq2.
- Updated `deseq` example object.

## DESeqAnalysis 0.1.7 (2019-01-31)

### Minor changes

- `contrastSamples`: Improved internal sample and contrast name handling.
  No longer attempting to force snake case formatting.
- Pinned lintr check package to steinbaugh fork.

## DESeqAnalysis 0.1.6 (2019-01-23)

### Minor changes

- Finish requiring `results` argument for all `DESeqAnalysis` methods, where
  applicable. For example, refer to `contrastSamples` in this release.
- Removed `sanitizeRowData` from imports.
- Draft update to internal `.matchResults` assert check, looking for `missing`
  results argument.

## DESeqAnalysis 0.1.5 (2019-01-14)

### Major changes

- Importing `DataFrame` to `tbl_df` coercion methods directly from [basejump][]
  instead of the [transformer][] package. Note that S4Transfomer package was
  renamed simply to transformer at this date.

### Minor changes

- Improved documentation for `DESeqAnalysis-class` and the corresponding
  generator function. Refer to the roxygen syntax (e.g. `rdname`) on how to
  accomplish this, since it is useful across packages that define S4 class.
- Improved working examples and removed todo/fixme comments.
- Tightened up [Travis CI][] and [AppVeyor CI][] config, which now only checks
  against master branch (e.g. for pull requests). This reduces the overall CI
  burden, so that pushes to develop branch don't trigger CI builds.

## DESeqAnalysis 0.1.4 (2019-01-08)

### Major changes

- Added new R Markdown template designed for a `DESeqAnalysis` object.

### Minor changes

- Switched to reexporting generics from [bioverbs][] instead of [basejump][],
  where applicable. For example, moved `contrastSamples` and `resultsTables`
  generics (verbs) to [bioverbs][].
- Removed internal `validateS4` code, instead relying upon `goalie::validate`.
- Simplified [Travis CI][] configuration to only test against `bioc-release`.
- Simplified internal asserts using `nullOK = TRUE`, where applicable. This was
  added in the corresponding release update to [goalie][].
- Now using `isAlpha` instead of `containsAlpha` for assert check.
- Split out NAMESPACE imports into a separate `imports.R` file.

## DESeqAnalysis 0.1.3 (2018-12-13)

### Major changes

- No longer using `results = 1L` by default in `DESeqAnalysis` methods for
  functions that support pulling a `DESeqResults` object. This can result in
  unwanted behavior, and we shouldn't assume use of first slotted contrast.
  Now the user must request the desired contrast explicitly.

### Minor changes

- Improved internal code for `plotDEGHeatmap`, `plotDEGPCA`,
- Code cleanup to pass lintr checks.
- Miscellaneous documentation (primarily working example) improvements.

## DESeqAnalysis 0.1.2 (2018-12-12)

### New functions

- `contrastSamples`: **Experimental**. Return the samples used to generate a
  corresponding `DESeqResults` object. May revise this approach to simply
  store the sample names directly in the `DESeqResults` in the future.
- `deg`: Utility function to obtain DEG gene vector.
- `resultsTables`: Migrated from [bcbioRNASeq][]. Enables easy output of
  directional DEG tables from `DESeqResults` object.

### Major changes

- Added new `.matchResults` internal utility to match `DESeqResults`.
- Reworked internal code for `export`.
- Removed `DESeqResultsTables` class.
- No longer exporting `DESeqResults` method for `plotDEGHeatmap`, `plotDEGPCA`.
  May add this back in a future release to maintain compatibility with
  [bcbioRNASeq][] workflow paper.

### Minor changes

- Miscellaneous documentation and assert check (using [goalie][]) fixes.
- Improved internal S4 validity checks using `goalie::validate` instead of
  `assertthat::validate_that`.
- Updated example `DESeqAnalysis` S4 data set (`deseq`).
- Reorganized and improved unit tests.
- Added a placeholder for `updateObject`, which must be added in future update.

## DESeqAnalysis 0.1.1 (2018-12-01)

### Major changes

- Improved `show` method for `DESeqResultsTables` class.

### Minor changes

- Miscellaneous documentation and working example improvements.
- Initial set up of [testthat][] unit tests.
- Added lintr check support.
- Enabled [Travis CI][] and [AppVeyor CI][] build checks.
- Added CI badges to README.

## DESeqAnalysis 0.1.0 (2018-11-19)

Initial release. Some of the functions defined in this package have been
migrated from my [bcbioRNASeq][] R package.

[acidplots]: https://acidplots.acidgenomics.com/
[Acid Genomics]: https://acidgenomics.com/
[AppVeyor CI]: https://www.appveyor.com/
[basejump]: https://basejump.acidgenomics.com/
[bcbioRNASeq]: https://bioinformatics.sph.harvard.edu/bcbioRNASeq/
[bioverbs]: https://bioverbs.acidgenomics.com/
[goalie]: https://goalie.steinbaugh.com/
[testthat]: http://testthat.r-lib.org/
[transformer]: https://transformer.acidgenomics.com/
[Travis CI]: https://travis-ci.com/
