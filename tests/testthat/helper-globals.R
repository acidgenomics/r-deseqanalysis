data(deseq, envir = environment())

dds <- as(deseq, "DESeqDataSet")
g2s <- Gene2Symbol(dds)
geneIds <- head(g2s[["geneId"]])
geneNames <- head(g2s[["geneName"]])

## nolint start
realpath <- basejump::realpath
## nolint end
