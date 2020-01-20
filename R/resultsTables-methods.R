#' @name resultsTables
#' @inherit acidgenerics::resultsTables
#' @note Updated 2019-12-18.
#'
#' @inheritParams acidroxygen::params
#' @inheritParams params
#' @param ... Additional arguments.
#'
#' @details
#' Generate tables summarizing the differential expression, with subsets for
#' differentially expressed genes (DEGs). DEG tables (i.e. everything except the
#' `all` table), are arranged by adjusted *P* value.
#'
#' @note It is generally recommended to not apply post hoc log fold change
#'   cutoffs. If a specific effect size is desired, instead run
#'   `DESeq2::results()` using the `lfcThreshold` parameter. Refer to the DESeq2
#'   documentation and vignette for details.
#'
#' @section Tables:
#'
#' - `all`: All genes, including genes without an adjusted *P* value. This table
#'   is unmodified, and the rows have not been re-arranged or subset. It is
#'   suitable for gene set enrichment analysis (GSEA).
#' - `up`: Upregulated genes.
#' - `down`: Downregulated genes.
#' - `both`: Bidirectional DEGs (up- and down-regulated). This table can be
#'   used for overrepresentation testing but should NOT be used for GSEA.
#'
#' @param extra `logical(1)`.
#'   Include row data and normalized counts from internal `DESeqDataSet`.
#' @param return `character(1)`.
#'   Type of data frame to return as a list.
#'   Uses [match.arg()][base::match.arg].
#'
#'   - `DataFrameList`: Returns `DataFrameList` with row names.
#'   - `tbl_df`: Returns `list` of `tbl_df` containing `"rowname"` column.
#'
#' @return `list`.
#' Named list containing subsets of `DESeqResults`.
#'
#' @examples
#' data(deseq)
#'
#' ## DESeqAnalysis ====
#' x <- resultsTables(deseq, i = 1L)
#' names(x)
#'
#' ## DESeqResults ====
#' ## Use of DESeqAnalysis is encouraged instead of this approach.
#' res <- results(deseq, i = 1L)
#' dds <- as(deseq, "DESeqDataSet")
#' x <- resultsTables(object = res, DESeqDataSet = dds)
#' names(x)
NULL



#' @rdname resultsTables
#' @name resultsTables
#' @importFrom acidgenerics resultsTables
#' @usage resultsTables(object, ...)
#' @export
NULL



## bcbioRNASeq v0.2 release series defaults:
## https://github.com/hbc/bcbioRNASeq/blob/v0.2.10/R/resultsTables-methods.R

## Note that this method is used in bcbioRNASeq F1000 paper.
## Updated 2019-11-12.
`resultsTables,DESeqResults` <-  # nolint
    function(
        object,
        DESeqDataSet = NULL,  # nolint
        alpha = NULL,
        lfcThreshold = NULL,
        return = c("tbl_df", "DataFrameList"),
        ...
    ) {
        validObject(object)
        assert(isAny(DESeqDataSet, c("DESeqDataSet", "NULL")))
        return <- match.arg(return)

        ## Legacy bcbioRNASeq arguments ----------------------------------------
        ## nocov start
        call <- match.call()
        ## dir
        if (isSubset("dir", names(call))) {
            stop(
                "'dir' argument is defunct.\n",
                "Use 'export()' instead."
            )
        }
        ## dropboxDir
        if (isSubset("dropboxDir", names(call))) {
            stop(
                "'dropboxDir' argument is defunct.\n",
                "Use 'copyToDropbox()' instead."
            )
        }
        ## headerLevel
        if (isSubset("headerLevel", names(call))) {
            stop("'headerLevel' argument is defunct.")
        }
        ## rdsToken
        if (isSubset("rdsToken", names(call))) {
            stop(
                "'rdsToken' argument is defunct.\n",
                "Use 'copyToDropbox()' instead."
            )
        }
        ## summary
        if (isSubset("summary", names(call))) {
            stop("'summary' argument is defunct.")
        }
        ## write
        if (isSubset("write", names(call))) {
            stop(
                "'write' argument is defunct.\n",
                "Use 'export()' instead."
            )
        }
        ## Error on unsupported arguments.
        assert(isSubset(
            x = setdiff(names(call), ""),
            y = names(formals())
        ))
        rm(call)
        ## nocov end

        ## Prepare results -----------------------------------------------------
        ## Join row data and counts from DESeqDataSet.
        if (is(DESeqDataSet, "DESeqDataSet")) {
            object <- .joinRowData(
                object = object,
                DESeqDataSet = DESeqDataSet
            )
            object <- .joinCounts(
                object = object,
                DESeqDataSet = DESeqDataSet
            )
        }
        ## Get the DEG character vectors, which we'll use against the rownames.
        both <- deg(
            object = object,
            alpha = alpha,
            lfcThreshold = lfcThreshold,
            direction = "both"
        )
        ## Early return if there are not DEGs.
        if (!hasLength(both)) {
            out <- list(all = object)
        } else {
            up <- deg(
                object = object,
                alpha = alpha,
                lfcThreshold = lfcThreshold,
                direction = "up"
            )
            down <- deg(
                object = object,
                alpha = alpha,
                lfcThreshold = lfcThreshold,
                direction = "down"
            )
            ## Prepare the return list.
            out <- list(
                all = object,
                up = object[up, , drop = FALSE],
                down = object[down, , drop = FALSE],
                both = object[both, , drop = FALSE]
            )
            ## Filter out empty up/down tables.
            out <- Filter(f = hasRows, x = out)
        }
        switch(
            EXPR = return,
            "DataFrameList" = DataFrameList(out),
            "tbl_df" = lapply(out, as_tibble)
        )
    }



#' @rdname resultsTables
#' @export
setMethod(
    f = "resultsTables",
    signature = signature("DESeqResults"),
    definition = `resultsTables,DESeqResults`
)



## Updated 2019-11-08.
`resultsTables,DESeqAnalysis` <-  # nolint
    function(
        object,
        i,
        lfcShrink = TRUE,
        extra = TRUE,
        alpha = NULL,
        lfcThreshold = NULL,
        return = c("tbl_df", "DataFrameList")
    ) {
        ## nocov start
        call <- match.call()
        ## results
        if ("results" %in% names(call)) {
            stop("'results' is defunct in favor of 'i'.")
        }
        assert(isSubset(
            x = setdiff(names(call), ""),
            y = names(formals())
        ))
        rm(call)
        ## nocov end
        validObject(object)
        assert(
            isFlag(lfcShrink),
            isFlag(extra)
        )
        return <- match.arg(return)
        ## Note that this will use the shrunken LFC values, if slotted.
        res <- results(object, i = i, lfcShrink = lfcShrink)
        ## Include extra annotations, if desired.
        if (isTRUE(extra)) {
            ## Get the DESeqDataSet, and humanize the sample names. Note that
            ## we're not calling `humanize()` here on the DESeqDataSet, because
            ## we want to keep the gene identifiers in the row names.
            dds <- as(object, "DESeqDataSet")
            ## Always attempt to use human-friendly sample names, defined by the
            ## `sampleName` column in `colData`. We're using this downstream
            ## when joining the normalized counts.
            dds <- convertSampleIDsToNames(dds)
        } else {
            dds <- NULL
        }
        resultsTables(
            object = res,
            DESeqDataSet = dds,
            alpha = alpha,
            lfcThreshold = lfcThreshold,
            return = return
        )
    }



#' @rdname resultsTables
#' @export
setMethod(
    f = "resultsTables",
    signature = signature("DESeqAnalysis"),
    definition = `resultsTables,DESeqAnalysis`
)
