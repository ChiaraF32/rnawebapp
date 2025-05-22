
#' Generate Sashimi Plot (PDF + PNG)
#'
#' This function generates a sashimi plot for a given sample using a specified GTF annotation
#' and a set of BAM files (1 patient + controls). It expands the region of interest, writes
#' a temporary TSV for `ggsashimi.py`, runs the command, and converts the resulting PDF to PNG
#' for Shiny visualization.
#'
#' @param result_object A list-like object (e.g. from OUTRIDER/FRASER results) containing `frares`
#'   with fields `sampleID` and `coord`, where `coord` is in the format `"chr:start-end"`.
#' @param sample_index Integer index indicating which sample (row of `result_object$frares`) to plot.
#' @param gtf_file Path to the GTF file used for annotation (e.g., `"./data/Homo_sapiens.GRCh38.113.gtf"`).
#' @param controls Character vector of sample IDs to use as controls (must match BAM filenames).
#' @param control_labels Character vector of labels corresponding to each control (same length as `controls`).
#' @param bam_dir Directory containing all BAM files, named as `<sampleID>.markdup.sorted.bam`.
#' @param output_dir Directory where the output PDF and PNG files should be saved. Default is current directory.
#' @param padding Number of base pairs to pad upstream and downstream of the target region. Default is 1000.
#' @param ggsashimi_path Path to the `ggsashimi.py` script. Default assumes it's in the working directory.
#'
#' @return A named list containing paths to:
#' \describe{
#'   \item{pdf}{The output PDF file of the sashimi plot.}
#'   \item{png}{The output PNG file converted from the PDF.}
#' }
#'
#' @import magick
#' @export
generate_sashimi_plot <- function(
    result_object,
    sample_index,
    controls,
    bam_dir,
    gtf_file = "./data/Homo_sapiens.GRCh38.113.gtf",
    control_labels = NULL,
    output_dir = "./generated/plots",
    padding = 1000,
    ggsashimi_path = "./ggsashimi.py"
) {
  library(ggplot2)
  library(data.table)
  library(gridExtra)
  library(magick)
  library(pdftools)

  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Check sample/coord availability
  if (nrow(result_object) == 0 || is.na(sample_index)) {
    warning("⚠️ result_object is empty or sample_index is NA.")
    return(NULL)
  }

  sample <- as.character(result_object$sampleID[sample_index])
  region <- as.character(result_object$coord[sample_index])

  if (is.na(sample) || is.na(region)) {
    warning("⚠️ Sample or region is NA.")
    return(NULL)
  }

  # Expand region
  region <- sub("^chr", "", region)
  region <- sub("^M", "MT", region)
  region_parts <- strsplit(region, "[:-]")[[1]]

  if (length(region_parts) != 3) {
    warning("❌ Region format not recognized: ", region)
    return(NULL)
  }

  chrom <- region_parts[1]
  start <- as.integer(region_parts[2])
  end   <- as.integer(region_parts[3])

  # Expand very small regions to minimum span
  min_span <- 100
  if (abs(end - start) < min_span) {
    midpoint <- floor((start + end) / 2)
    start <- max(1, midpoint - min_span)
    end <- midpoint + min_span
    message(glue::glue("🛠️ Region too small, expanded to: {chrom}:{start}-{end}"))
  }

  start <- max(1, start - padding)
  end   <- end + padding
  region_expanded <- paste0(chrom, ":", start, "-", end)

  # Prepare file paths
  sample_bam <- file.path(bam_dir, paste0(sample, ".markdup.sorted.bam"))
  control_bams <- file.path(bam_dir, paste0(controls, ".markdup.sorted.bam"))

  bam_files <- c(sample_bam, control_bams)
  samples <- c(sample, controls)

  # Check for existing BAMs
  bam_exists <- file.exists(bam_files)
  if (!any(bam_exists)) {
    warning("❌ No available BAM files found for: ", paste(bam_files, collapse = ", "))
    return(NULL)
  }

  # Only include available BAMs
  bam_files <- bam_files[bam_exists]
  samples <- samples[bam_exists]
  groups <- c("Patient", rep("Control", length(controls)))[bam_exists]

  # Assemble table
  sashimi_df <- data.frame(samples = samples, bam_files = bam_files, groups = groups, stringsAsFactors = FALSE)

  sashimi_input <- tempfile(fileext = ".tsv")
  on.exit(unlink(sashimi_input), add = TRUE)
  write.table(sashimi_df, file = sashimi_input, sep = "\t", quote = FALSE, row.names = FALSE, col.names = FALSE)

  output_pdf <- file.path(output_dir, paste0(sample, "_", region, "_sashimi_plot.pdf"))
  output_png <- sub("\\.pdf$", ".png", output_pdf)

  # Build sashimi command
  cmd <- paste(
    ggsashimi_path,
    "-b", sashimi_input,
    "-c", region_expanded,
    "-g", gtf_file,
    "-o", output_pdf,
    "-M 10 -C 3 -O 3 --alpha 0.25 -A median_j",
    "--base-size=20 --scale-ann-height --height=5 --width=18 --shrink --fix-y-scale"
  )

  # Run sashimi
  system(cmd)

  # Check output existence
  if (!file.exists(output_pdf)) {
    warning("❌ Sashimi plot PDF not generated: ", output_pdf)
    return(NULL)
  }

  img <- image_read_pdf(output_pdf, density = 150)
  image_write(img, output_png, format = "png")

  return(list(pdf = output_pdf, png = output_png))
}

#' Generate a faceted violin-boxplot of gene expression with highlighted sample and padj labels
#'
#' @description
#' This function visualizes normalized expression values for one or more genes from an OUTRIDER dataset
#' across all samples. Each gene is displayed in a separate facet with violin and boxplots to show distribution.
#' A specified sample is highlighted in red, with its adjusted p-value (`padj`) displayed as a label above the point.
#'
#' @param ods An OUTRIDER dataset (SummarizedExperiment object)
#' @param genes A character vector of gene symbols or Ensembl IDs to plot
#' @param sampleID A character string specifying the sample ID to highlight
#'
#' @return A ggplot2 object showing expression distributions with violin and boxplots, faceted by gene,
#'         with the specified sample highlighted and labeled by its padj value
#' @export
#'
#' @importFrom dplyr filter mutate
#' @importFrom tidyr gather
#' @importFrom ggplot2 ggplot geom_violin geom_boxplot geom_point geom_text facet_wrap theme element_blank labs aes
#' @importFrom SummarizedExperiment rownames
#' @importFrom OUTRIDER counts padj

plot_gene_expression_multi <- function(ods, genes, sampleID) {
  # Extract and reshape normalized counts and padj
  counts_norm <- as.data.frame(counts(ods, normalized = TRUE))
  counts_norm$geneID <- rownames(counts_norm)

  padj <- as.data.frame(padj(ods))
  padj$geneID <- rownames(padj)

  long_counts <- counts_norm %>%
    tidyr::gather("sample_ID", "counts", -geneID) %>%
    dplyr::filter(geneID %in% genes)

  long_padj <- padj %>%
    tidyr::gather("sample_ID", "padj", -geneID) %>%
    dplyr::filter(geneID %in% genes)

  merged_long <- dplyr::left_join(long_counts, long_padj, by = c("geneID", "sample_ID"))

  # Highlighted sample
  highlight <- dplyr::filter(merged_long, sample_ID == sampleID)
  other <- dplyr::filter(merged_long, sample_ID != sampleID)

  # Round padj for display
  highlight <- highlight %>%
    dplyr::mutate(padj_label = sprintf("padj=%.3g", padj))

  # Plot
  p <- ggplot(other, aes(x = factor(0), y = counts)) +
    facet_wrap(~geneID, scales = "free_y") +
    geom_violin(fill = "white") +
    geom_boxplot(width = 0.3, outlier.shape = NA) +
    geom_point(data = highlight, aes(x = factor(0), y = counts), color = "red", size = 2) +
    geom_text(data = highlight, aes(x = factor(0), y = counts, label = padj_label), color = "red", hjust = -0.2, vjust = -0.2, size = 3.5) +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x = element_blank()) +
    labs(x = NULL, y = "Expression (normalized counts)", title = paste("Expression for", paste(genes, collapse = ", ")))

  return(p)
}

