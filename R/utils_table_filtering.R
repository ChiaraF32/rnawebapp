
filtered_annotated_table <- function(results_tbl, genes) {
  dplyr::filter(results_tbl, geneID %in% genes)
}
