# Here's the potential update method for results that aren't named.
# # TODO Require DESeqAnalysis to have named results, so we can remove this.
# if (!has_names(list)) {
#     names(list) <- makeNames(vapply(
#         X = list,
#         FUN = contrastName,
#         FUN.VALUE = character(1L)
#     ))
# }
