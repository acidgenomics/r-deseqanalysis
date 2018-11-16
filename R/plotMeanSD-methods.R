plotMeanSD.DESeqDataSet <-  # nolint
    function(
        object,
        vst = TRUE,
        rlog = FALSE,
        legend = getOption("basejump", FALSE)
    ) {
        validObject(object)
        assert_is_a_bool(vst)
        if (isTRUE(vst)) {
            vst <- assay(varianceStabilizingTransformation(object))
        } else {
            vst <- NULL
        }
        assert_is_a_bool(rlog)
        if (isTRUE(rlog)) {
            rlog <- assay(rlog(object))
        } else {
            rlog <- NULL
        }
        .plotMeanSD(
            raw = counts(object, normalized = FALSE),
            normalized = counts(object, normalized = TRUE),
            vst = vst,
            rlog = rlog,
            legend = legend
        )
    }



#' @rdname plotMeanSD
#' @export
setMethod(
    f = "plotMeanSD",
    signature = signature("DESeqDataSet"),
    definition = plotMeanSD.DESeqDataSet
)



# DESeqAnalysis ================================================================
#' @rdname plotMeanSD
#' @export
setMethod(
    f = "plotMeanSD",
    signature = signature("DESeqAnalysis"),
    definition = function(object) {
        plotMeanSD(as(object, "DESeqDataSet"))
    }
)
