filtered_expression_genes <- function(ods) {
  norm_counts <- SummarizedExperiment::assay(ods, normalized = TRUE)
  rownames(norm_counts)[rowSums(norm_counts) > 0]
}
