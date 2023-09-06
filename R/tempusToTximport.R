## FIXME Need to rework to handle expansion of zero-count transcripts.



#' Import Tempus xR transcript-level counts into tximport
#'
#' @export
#' @note Updated 2023-08-31.
#'
#' @param file `character(1)`.
#' Tempus transcript-level counts file.
#'
#' @return tximport `list`.
#'
#' @examples
#' ## > file <- "g_rna_abundance.csv.gz"
#' ## > txi <- tempusToTximport(file)
tempusToTximport <- function(file) {
    assert(
        requireNamespaces("tximport"),
        isString(file)
    )
    df <- import(file)
    df <- as(df, "DFrame")
    assert(identical(
        x = colnames(df),
        y = c(
            "patient_id",
            "analysis_id",
            "transcript_code",
            "transcript_length",
            "eff_transcript_length",
            "raw_transcript_count",
            "transcript_tpm"
        )
    ))
    f <- paste(df[["patient_id"]], df[["analysis_id"]], sep = "_")
    f <- gsub(pattern = "-", replacement = "_", x = f, fixed = TRUE)
    colnames(df)[colnames(df) == "transcript_code"] <- "target_id"
    colnames(df)[colnames(df) == "transcript_length"] <- "length"
    colnames(df)[colnames(df) == "eff_transcript_length"] <- "eff_length"
    colnames(df)[colnames(df) == "raw_transcript_count"] <- "est_counts"
    colnames(df)[colnames(df) == "transcript_tpm"] <- "tpm"
    cols <- c("target_id", "length", "eff_length", "est_counts", "tpm")
    df <- df[, cols]
    spl <- split(x = df, f = f)
    assert(is(spl, "CompressedSplitDFrameList"))
    txIds <- sort(Reduce(f = intersect, x = spl[, "target_id"]))
    spl <- SplitDataFrameList(lapply(
        X = spl,
        FUN = function(df) {
            idx <- match(x = txIds, table = df[["target_id"]])
            df <- df[idx, , drop = FALSE]
            df
        }
    ))
    tmpdir <- tempdir2()
    files <- Map(
        object = spl,
        name = names(spl),
        MoreArgs = list(dir = tmpdir),
        f = function(object, name, dir ) {
            export(
                object = object,
                con = file.path(dir, paste0(name, ".tsv")),
                quiet = TRUE
            )
        }
    )
    files <- unlist(files, recursive = FALSE, use.names = TRUE)
    txi <- tximport::tximport(
        files = files,
        type = "kallisto",
        txIn = TRUE,
        txOut = TRUE,
        countsFromAbundance = "no",
        tx2gene = NULL,
        ignoreTxVersion = FALSE
    )
    unlink2(tmpdir)
    txi
}
