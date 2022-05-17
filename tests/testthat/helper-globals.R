## nolint start
Gene2Symbol <- AcidGenomes::Gene2Symbol
realpath <- basejump::realpath
render <- rmarkdown::render
## nolint end

tmpenv <- new.env()
data(deseq, deseqMinimal, envir = tmpenv)
objs <- list()
objs[["deseq"]] <- get("deseq", envir = tmpenv)
objs[["deseqMinimal"]] <- get("deseqMinimal", envir = tmpenv)
rm(tmpenv)
objs[["dds"]] <- as(objs[["deseq"]], "DESeqDataSet")
objs[["g2s"]] <- Gene2Symbol(objs[["dds"]])
objs[["geneIds"]] <- head(objs[["g2s"]][["geneId"]])
objs[["geneNames"]] <- head(objs[["g2s"]][["geneName"]])
