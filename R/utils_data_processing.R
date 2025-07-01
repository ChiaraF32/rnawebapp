
#' Annotate Ensembl IDs with HGNC symbols
#'
#' @param se A SummarizedExperiment or DESeqDataSet object
#' @param annotation A data frame containing `ensgene` and `symbol` columns (default: annotables::grch38)
#'
#' @return A modified object with ENSG IDs and HGNC rownames where available
#' @export
#'
#' @importFrom SummarizedExperiment mcols
#' @importFrom data.table data.table
annotate_ensembl_ids <- function(se, annotation = annotables::grch38) {
  if (is.null(rownames(se))) {
    stop("Input object must have rownames containing Ensembl IDs.")
  }

  # Strip versioning from Ensembl IDs (e.g. ENSG000001.1 -> ENSG000001)
  geneIDs <- gsub("\\.[0-9]*(_[0-9]*)?.*$", "", rownames(se))

  # Merge with annotation
  merged_annot <- merge(
    data.table(ensgene = geneIDs),
    annotation,
    sort = FALSE,
    all.x = TRUE
  )[!duplicated(ensgene), ]

  # Set gene IDs
  if (!"geneID" %in% colnames(SummarizedExperiment::mcols(se))) {
    SummarizedExperiment::mcols(se)$ENSG <- geneIDs

    # Replace rownames where possible
    new_names <- ifelse(
      is.na(merged_annot$symbol) |
        merged_annot$symbol == "" |
        duplicated(merged_annot$symbol),
      geneIDs,
      merged_annot$symbol
    )

    rownames(se) <- new_names
  }

  return(se)
}

#' Generate OUTRIDER and FRASER results with optional padjust filtering
#'
#' @param ods An OUTRIDER dataset (ODS)
#' @param fds A FRASER dataset (FDS)
#' @param padj_out Adjusted p-value threshold for OUTRIDER results (default = 0.05)
#' @param padj_fra Adjusted p-value threshold for FRASER results (default = 0.05)
#' @param merged Logical; whether to return a merged data.frame containing overlapping samples and genes (default = TRUE)
#' @param all Logical; whether to also return unfiltered result tables from OUTRIDER and FRASER (default = FALSE)
#'
#' @return A list containing filtered `outres` and `frares`, and optionally `merged`, `outres_all`, `frares_all`
#' @export
#'
#' @importFrom dplyr select rename transmute filter
#' @importFrom data.table as.data.table
#' @importFrom OUTRIDER results
#' @importFrom FRASER results
generate_results <- function(ods, fds, padj_out = 0.05, padj_fra = 0.05, merged = TRUE, all = FALSE) {

  # Get filtered results
  outres <- as.data.frame(OUTRIDER::results(ods, padjCutoff = padj_out))
  frares <- as.data.table(FRASER::results(fds, padjCutoff = padj_fra))

  # Clean up and standardize FRASER results
  frares <- frares %>%
    dplyr::rename(geneID = hgncSymbol) %>%
    transmute(
      sampleID, geneID,
      coord = paste0(seqnames, ":", start, "-", end),
      type, strand, padjust, psiValue, deltaPsi,
      counts, totalCounts, meanCounts, meanTotalCounts, annotatedJunction
    )

  # Rearrange OUTRIDER results to put padjust early
  outres <- outres %>%
    dplyr::select(sampleID, geneID, padjust, everything())

  # Optional merged results
  merged_df <- NULL
  if (merged) {
    merged_df <- merge(
      frares, outres,
      by = c("sampleID", "geneID"),
      suffixes = c(".fra", ".out")
    ) %>%
      dplyr::select(geneID, sampleID) %>%
      unique()
  }

  # Optional full results (unfiltered)
  outres_all <- NULL
  frares_all <- NULL
  if (all) {
    outres_all <- as.data.frame(OUTRIDER::results(ods, all = TRUE))
    outres_all <- outres_all %>%
      dplyr::select(sampleID, geneID, padjust, everything())
    frares_all <- as.data.table(FRASER::results(fds, all = TRUE))
    frares_all <- frares_all %>%
      dplyr::rename(geneID = hgncSymbol) %>%
      transmute(
        sampleID, geneID,
        coord = paste0(seqnames, ":", start, "-", end),
        type, strand, padjust, psiValue, deltaPsi,
        counts, totalCounts, meanCounts, meanTotalCounts, annotatedJunction
      )
  }

  # Return result list
  return(list(
    outres = outres,
    frares = frares,
    merged = merged_df,
    outres_all = outres_all,
    frares_all = frares_all
  ))
}


#' Update BAM file paths in an OUTRIDER or FRASER object using a samplesheet
#'
#' This function updates the `RNA_BAM_FILE` and `bamFile` fields in the `colData`
#' of an OUTRIDER or FRASER dataset object using a provided samplesheet.
#' Matching is performed by comparing BAM filenames (i.e., basename of the path).
#'
#' @param object An OUTRIDER or FRASER dataset object (e.g., `ods` or `fds`)
#' @param samplesheet A data frame containing a column `RNA_BAM_FILE` with updated BAM file paths
#'
#' @return The updated object with `colData(object)$RNA_BAM_FILE` and `colData(object)$bamFile` corrected
#' @export
#'
#' @import Rsamtools
#' @importFrom SummarizedExperiment colData
update_bam_paths <- function(object, samplesheet) {
  # Extract base filenames
  object_files <- basename(colData(object)$RNA_BAM_FILE)
  samplesheet_files <- basename(samplesheet$RNA_BAM_FILE)

  # Match filenames
  matched_indices <- match(object_files, samplesheet_files)

  if (any(is.na(matched_indices))) {
    warning("Some BAM files in the object could not be matched to the samplesheet and will be set to NA.")
  }

  # Assign full paths where matched
  new_paths <- samplesheet$RNA_BAM_FILE[matched_indices]
  SummarizedExperiment::colData(object)$RNA_BAM_FILE <- new_paths
  SummarizedExperiment::colData(object)$bamFile <- Rsamtools::BamFileList(new_paths)

  return(object)
}

#' Generate a summary table of aberrant genes detected per sample
#'
#' This function returns a data frame summarizing the number of aberrant genes
#' detected in each sample by the OUTRIDER and FRASER datasets.
#' If a sample is not present in one of the datasets, it is assigned a count of 0.
#'
#' @param ods An OUTRIDER dataset object.
#' @param fds A FRASER dataset object.
#'
#' @return A data.frame with one row per sample and the number of aberrant genes
#' detected by OUTRIDER (`OUTRIDER_outliers`) and FRASER (`FRASER_outliers`).
#' Samples missing in a dataset will have 0 as their outlier count.
#'
#' @export
#'
#' @importFrom dplyr select rename transmute filter
#' @importFrom OUTRIDER results
#' @importFrom FRASER results
summarise_data <- function(ods, fds) {
  # Retrieve sample names
  ods_samples <- colnames(ods)
  fds_samples <- colnames(fds)

  # Get number of aberrant genes per sample
  ods_aberrant <- aberrant(ods, by = "sample")
  fds_aberrant <- aberrant(fds, by = "sample")

  # Get list of all unique samples
  all_samples <- sort(unique(c(ods_samples, fds_samples)))

  # Merge into one data frame
  sample_status <- data.frame(
    sampleID = all_samples,
    OUTRIDER_outliers = as.integer(ods_aberrant[all_samples]),
    FRASER_outliers = as.integer(fds_aberrant[all_samples]),
    stringsAsFactors = FALSE
  )

  # Replace NA with 0 (for samples not present in one of the datasets)
  sample_status$OUTRIDER_aberrant[is.na(sample_status$OUTRIDER_aberrant)] <- 0
  sample_status$FRASER_aberrant[is.na(sample_status$FRASER_aberrant)] <- 0

  return(sample_status)
}


#' Annotate gene-level results with OMIM phenotypes and GO terms
#'
#' This function takes a data frame of gene-level results (e.g., from OUTRIDER or FRASER)
#' and optionally adds OMIM phenotype annotations and Gene Ontology (GO) term annotations.
#' It uses `org.Hs.eg.db` for OMIM links and `biomaRt` to retrieve GO terms via Ensembl.
#'
#' @param results A data.frame containing gene-level results with a column named `geneID`
#'        corresponding to HGNC gene symbols (e.g., as used in OUTRIDER or FRASER).
#' @param add_omim Logical. Whether to annotate genes with OMIM phenotype data. Default is TRUE.
#' @param omim_file Path to the OMIM phenotype annotation file (TSV format). Default is `"./data/omim-phenotype.txt"`.
#'        The file must include at least the columns: `OMIM` and `Phenotypes`.
#' @param add_go Logical. Whether to annotate genes with Gene Ontology (GO) terms. Default is TRUE.
#' @param go_source The BioMart source for retrieving GO terms. Default is `"ensembl"`.
#'
#' @return A data.frame with the original results, plus additional columns:
#'         - `Phenotypes`: OMIM phenotype(s) associated with each gene (if `add_omim = TRUE`)
#'         - `GO_TERMS`: GO term descriptions associated with each gene (if `add_go = TRUE`)
#'
#' @importFrom dplyr group_by summarise %>%
#' @importFrom biomaRt useMart getBM keys select
#' @importFrom org.Hs.eg.db org.Hs.eg.db
#' @export
#'
## Retrieve OMIM data

annotate_results_with_omim_go <- function(results,
                                          add_omim = TRUE,
                                          omim_file = system.file("extdata", "omim-phenotype.txt", package = "rnawebapp"),
                                          add_go = TRUE,
                                          go_source = "ensembl") {

  # Start with the original results
  annotated <- results

    # ----- OMIM ANNOTATION -----
  if (add_omim) {
    message("🔍 Annotating with OMIM...")

    phe <- read.table(omim_file, header = TRUE, fill = TRUE)
    colnames(phe)[1] <- "OMIM"

    cols <- c("SYMBOL", "GENENAME", "OMIM")

    # Get all gene symbols (HGNC symbols)
    gene_symbols <- keys(org.Hs.eg.db, keytype = "ENSEMBL")

    omim <- biomaRt::select(org.Hs.eg.db, keys = gene_symbols, columns = cols, keytype = "ENSEMBL")
    colnames(omim)[1] <- "ensembl"

    mim_phe <- merge(omim, phe, by = "OMIM", all.x = TRUE)

    sum_ens <- mim_phe %>%
      group_by(SYMBOL) %>%
      summarise(Phenotypes = paste(unique(Phenotypes), collapse = "; "), .groups = "drop")

    annotated <- merge(annotated, sum_ens, by.x = "geneID", by.y = "SYMBOL", all.x = TRUE)
  }

  # ----- GO TERM ANNOTATION -----
  if (add_go) {
    message("🔍 Annotating with GO terms via BioMart...")

    mart <- useMart(go_source, dataset = "hsapiens_gene_ensembl")
    go_terms <- getBM(
      mart = mart,
      attributes = c("hgnc_symbol", "go_id", "namespace_1003", "name_1006"),
      filters = "hgnc_symbol",
      values = annotated$geneID
    )

    go_terms <- go_terms[!duplicated(go_terms), ]
    go_terms <- go_terms[!(is.na(go_terms$name_1006) | go_terms$name_1006 == ""), ]

    go <- go_terms %>%
      group_by(hgnc_symbol) %>%
      summarise(GO_TERMS = paste(unique(name_1006), collapse = " | "), .groups = "drop")

    annotated <- merge(annotated, go, by.x = "geneID", by.y = "hgnc_symbol", all.x = TRUE)
  }

  # Sort by padjust (if present)
  if ("padjust" %in% colnames(annotated)) {
    annotated <- annotated[order(annotated$padjust), ]
  }

  return(annotated)
}

#' Plot phenotype distribution
#'
#' @description Creates a bar plot showing the count of each phenotype in the samplesheet.
#'
#' @param samplesheet A data.frame containing a column named 'PHENOTYPE'
#'
#' @return A ggplot2 object showing phenotype counts
#' @export
#'
#' @importFrom ggplot2 ggplot aes geom_bar theme_minimal labs element_text
plot_phenotype_distribution <- function(samplesheet) {
  if (!"PHENOTYPE" %in% colnames(samplesheet)) {
    stop(paste0(
      "The samplesheet must contain a 'PHENOTYPE' column. Found columns: ",
      paste(colnames(samplesheet), collapse = ", ")
    ))
  }

  ggplot(samplesheet, aes(x = PHENOTYPE)) +
    geom_bar(fill = "steelblue") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
      axis.text.y = element_text(size = 12),
      axis.title = element_text(size = 13)
    ) +
    labs(
      title = "",
      x = "Phenotype",
      y = "Count"
    )
}


#' Merge OUTRIDER and FRASER Results by Gene and Sample
#'
#' This function merges two data frames (`ores` and `frares`) containing OUTRIDER and FRASER results,
#' respectively. It merges them on `geneID` and `sampleID`, identifies duplicates, and returns a
#' collapsed data frame with a count of how many rows were merged per `(geneID, sampleID)` pair.
#'
#' @param ores A data frame of OUTRIDER results with `geneID` and `sampleID` columns.
#' @param frares A data frame of FRASER results with `geneID` and `sampleID` columns.
#'
#' @return A data frame with columns: `geneID`, `sampleID`, and `n_merged` (number of merged entries).
#'
#' @examples
#' merge_outrider_fraser(ores_df, frares_df)

merge_outrider_fraser <- function(ores, frares) {
  merged <- merge(
    ores, frares,
    by = c("geneID", "sampleID"),
    suffixes = c(".out", ".fra")
  )

  collapsed <- merged %>%
    dplyr::group_by(geneID, sampleID) %>%
    dplyr::summarise(
      splice_events = dplyr::n(),  # Count how many duplicates were merged
      .groups = "drop"
    )

  return(collapsed)
}


validate_padj <- function(val) {
  isTRUE(suppressWarnings(!is.na(as.numeric(val)) & as.numeric(val) >= 0 & as.numeric(val) <= 1))
}
