
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

#' Fix the FRASER results directory
#'
#' @param fds A FRASER dataset
#' @param base_path The path to the directory containing the `.h5` objects (default: "./data/savedObjects/MUSCLE--v38/")
#' @param working_dir The path to the working directory
#' @return A modified object with appropriate h5 paths
#' @export
#'
fixFdsH5Paths <- function(fds,
                          base_path = "./data/savedObjects/MUSCLE--v38/",
                          working_dir = "./data/",
                          verbose = FALSE) {
  fds@workingDir <- working_dir

  # Fix spliced assays
  for (assay_name in names(fds@assays@data@listData)) {
    h5_path <- file.path(base_path, paste0(assay_name, ".h5"))
    fds@assays@data@listData[[assay_name]]@seed@seed@filepath <- h5_path
    if (verbose) message("✔️ Spliced assay ", assay_name, ": ", h5_path)
  }

  # Fix non-spliced assays
  for (assay_name in names(fds@nonSplicedReads@assays@data@listData)) {
    h5_path <- file.path(base_path, paste0(assay_name, ".h5"))
    fds@nonSplicedReads@assays@data@listData[[assay_name]]@seed@seed@filepath <- h5_path
    if (verbose) message("✔️ Non-spliced assay ", assay_name, ": ", h5_path)
  }

  return(fds)
}

#' Generate OUTRIDER and FRASER results with optional padjust filtering
#'
#' @param fds A FRASER dataset
#' @param ods An OUTRIDER dataset
#' @param padj_threshold Optional adjusted p-value threshold for filtering (default = 0.05)
#' @param merged select to return a dataframe containing the samples and genes in both FRASER and OUTRIDER
#'
#' @return list containing OUTRIDER and FRASER result tables
#' @export
#'
#' @importFrom dplyr select rename transmute filter
#' @importFrom data.table as.data.table
#' @importFrom OUTRIDER results
#' @importFrom FRASER results
generate_results <- function(ods, fds, padj_threshold = 0.05, merged = TRUE) {
  outres <- as.data.frame(OUTRIDER::results(ods, padjCutoff = padj_threshold))
  frares <- as.data.table(FRASER::results(fds, padjCutoff = padj_threshold))

  frares <- frares %>%
    dplyr::rename(geneID = hgncSymbol) %>%
    transmute(
      sampleID, geneID,
      coord = paste0(seqnames, ":", start, "-", end),
      type, strand, padjust, psiValue, deltaPsi, counts, totalCounts, meanCounts, meanTotalCounts, annotatedJunction
    )

  outres <- outres %>%
    dplyr::select(sampleID, geneID, padjust, everything())

  merged <- merge(frares, outres, by.x = c("geneID", "sampleID"), by.y = c("geneID", "sampleID"), suffixes = c(".fra", ".out")) %>%
    dplyr::select(geneID:sampleID) %>%
    unique()

  return(list(outres = outres, frares = frares, merged = merged))
}


#' Update BAM file paths for OUTRIDER or FRASER objects
#'
#' @param object An OUTRIDER or FRASER dataset object (ods or fds)
#' @param new_base_path The new directory where BAM files are located
#'
#' @return The updated dataset object with corrected BAM file paths
#' @export
#'
#' @import Rsamtools
#' @importFrom SummarizedExperiment colData mcols
update_bam_paths <- function(object, new_base_path) {
  if (!dir.exists(new_base_path)) {
    stop("❌ Provided base path does not exist: ", new_base_path)
  }

  if (!"RNA_BAM_FILE" %in% colnames(colData(object))) {
    stop("❌ Input object does not contain a 'RNA_BAM_FILE' column in colData.")
  }

  # Update BAM paths
  updated_bam_paths <- file.path(new_base_path, basename(SummarizedExperiment::colData(object)$RNA_BAM_FILE))
  SummarizedExperiment::colData(object)$RNA_BAM_FILE <- updated_bam_paths
  SummarizedExperiment::colData(object)$bamFile <- Rsamtools::BamFileList(updated_bam_paths)

  return(object)
}

#' Generate a table summarising available data and samples
#'
#' @param fds A FRASER dataset
#' @param ods An OUTRIDER dataset
#'
#' @return data.frame with which data is available for each sample
#' @export
#'
#' @importFrom dplyr select rename transmute filter
#' @importFrom OUTRIDER results
#' @importFrom FRASER results
summarise_data <- function(ods, fds) {
  # Retrieve sample names
  ods_samples <- colnames(ods)
  fds_samples <- colnames(fds)

  # Get list of all unique samples
  all_samples <- sort(unique(c(ods_samples, fds_samples)))

  # Merge into one data frame
  sample_status <- data.frame(
    sampleID = all_samples,
    OUTRIDER = all_samples %in% ods_samples,
    FRASER = all_samples %in% fds_samples,
    stringsAsFactors = FALSE
  )

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

