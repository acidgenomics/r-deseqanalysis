## nolint start
GeneToSymbol <- AcidGenomes::GeneToSymbol
`rowData<-` <- SummarizedExperiment::`rowData<-`
`rowRanges<-` <- SummarizedExperiment::`rowRanges<-`
data <- utils::data
realpath <- AcidBase::realpath
render <- rmarkdown::render
tempdir2 <- AcidBase::tempdir2
unlink2 <- AcidBase::unlink2
## nolint end

tmpenv <- new.env()
data(deseq, deseqMinimal, envir = tmpenv)
objs <- list()
objs[["deseq"]] <- get("deseq", envir = tmpenv)
objs[["deseqMinimal"]] <- get("deseqMinimal", envir = tmpenv)
rm(tmpenv)
objs[["dds"]] <- as(objs[["deseq"]], "DESeqDataSet")
objs[["g2s"]] <- GeneToSymbol(objs[["dds"]])
objs[["geneIds"]] <- head(objs[["g2s"]][["geneId"]])
objs[["geneNames"]] <- head(objs[["g2s"]][["geneName"]])
