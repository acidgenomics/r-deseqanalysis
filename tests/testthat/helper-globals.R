data(deseq, envir = environment())

dds <- as(deseq, "DESeqDataSet")
g2s <- basejump::Gene2Symbol(dds)
geneIds <- head(g2s[["geneId"]])
geneNames <- head(g2s[["geneName"]])

## nolint start
realpath <- basejump::realpath
render <- rmarkdown::render
## nolint end
