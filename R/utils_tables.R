#' Filter Annotated Results Table by Gene and/or Sample
#'
#' Filters an annotated results data frame (e.g., from OUTRIDER or FRASER)
#' by specified gene symbols and/or sample IDs.
#'
#' @param results_tbl A data frame containing at least `geneID` and `sampleID` columns.
#'        Typically derived from FRASER/OUTRIDER results with annotation.
#' @param genes Optional character vector of gene symbols to retain (matching `geneID` column).
#' @param samples Optional character vector of sample IDs to retain (matching `sampleID` column).
#'
#' @return A filtered `data.frame` containing only rows that match the given gene(s) and/or sample(s).
#'
#' @examples
#' # Filter by gene only
#' filtered_annotated_table(results_tbl, genes = c("DMD", "ACTA1"))
#'
#' # Filter by sample only
#' filtered_annotated_table(results_tbl, samples = "IVCT-38-F")
#'
#' # Filter by both
#' filtered_annotated_table(results_tbl, genes = "MYH1", samples = "IVCT-43Y-M")
#'
#' @export
filtered_annotated_table <- function(results_tbl, genes = NULL, samples = NULL) {
  filtered <- results_tbl

  if (!is.null(genes)) {
    filtered <- dplyr::filter(filtered, geneID %in% genes)
  }

  if (!is.null(samples)) {
    filtered <- dplyr::filter(filtered, sampleID %in% samples)
  }

  return(filtered)
}


#' Render a Gene-Level Results Table with Fixed Columns
#'
#' Renders a `DT::datatable` for annotated gene-level results (e.g., OUTRIDER or FRASER)
#' with selected columns wrapped for display.
#'
#' @param data_type A `data.frame` or `tibble` containing gene-level results with annotation.
#'        Must contain `GO_TERMS` and `Phenotypes` columns.
#'
#' @return A Shiny render function that outputs a `DT::datatable`.
#'
#' @seealso \code{\link[DT]{renderDT}}, \code{\link{filtered_annotated_table}}, \code{\link{util_nowrap_dt}}
#'
#' @export
render_gene_table <- function(data_type, sample_id) {
  renderDT({
    req(sample_id())
    filtered <- filtered_annotated_table(data_type, samples = sample_id())
    util_nowrap_dt(filtered, nowrap_columns = c("GO_TERMS", "Phenotypes"))
  })
}
