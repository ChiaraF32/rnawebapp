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

#' Render RNA fusion table for a selected sample
#'
#' This function searches for a fusion results file in a samplesheet that matches
#' the provided sample ID (by filename), reads it as a tibble, and displays it in
#' an interactive datatable.
#'
#' It is intended for use inside a Shiny app, where `sample_id` is a reactive expression.
#'
#' @param samplesheet A data frame with a `FUSIONS` column containing paths to fusion result files
#' @param sample_id A reactive expression returning the current sample ID to display
#'
#' @return A Shiny `renderDT()` expression that displays the fusion results table
#' @export
#'
#' @importFrom DT renderDT datatable
#' @importFrom readr read_tsv cols
#' @importFrom shiny req validate
render_rna_fusions <- function(samplesheet, sample_id) {
  renderDT({
    req(sample_id())

    # Match the sample ID to the FUSIONS file
    matches <- grep(sample_id(), basename(samplesheet$FUSIONS), value = TRUE)

    if (length(matches) == 0) {
      validate("❌ No matching fusion file found for this sample.")
    } else if (length(matches) > 1) {
      warning(paste("⚠️ Multiple matching fusion files found for", sample_id(), "- using first match."))
    }

    path <- samplesheet$FUSIONS[basename(samplesheet$FUSIONS) %in% matches[1]]
    df <- readr::read_tsv(path, col_types = cols())
    util_nowrap_dt(df)
  })
}
