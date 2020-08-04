data(deseq, envir = environment())

dds <- as(deseq, "DESeqDataSet")
g2s <- Gene2Symbol(dds)
geneIDs <- head(g2s[["geneID"]])
geneNames <- head(g2s[["geneName"]])

## nolint start
realpath <- basejump::realpath
## nolint end
