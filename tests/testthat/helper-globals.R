data(deseq, envir = environment())

dds <- as(deseq, "DESeqDataSet")
vst <- as(deseq, "DESeqTransform")
res <- as(deseq, "DESeqResults")

g2s <- Gene2Symbol(dds)
geneIDs <- head(g2s[["geneID"]])
geneNames <- head(g2s[["geneName"]])
