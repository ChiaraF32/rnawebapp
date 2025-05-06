
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
    gtf_file = "./data/Homo_sapiens.GRCh38.113.gtf",
    controls = c("IVCT-43Y-M", "IVCT-38-F"),
    control_labels = c("Control", "Control"),
    bam_dir = "/Volumes/PERKINS-LL-001/Sequencing/rnaseq/secondary/nfcore/2024-12-16_GenomicsWA_Realigned/star_salmon/",
    output_dir = ".",
    padding = 1000,
    ggsashimi_path = "./ggsashimi.py"
) {

  library(ggplot2)
  library(data.table)
  library(gridExtra)
  library(magick)
  library(pdftools)

  sample <- as.character(result_object$frares$sampleID[sample_index])
  region <- as.character(result_object$frares$coord[sample_index])

  region <- sub("^chr", "", region)
  region <- sub("^M", "MT", region)

  region_parts <- strsplit(region, "[:-]")[[1]]
  chrom <- region_parts[1]
  start <- as.integer(region_parts[2])
  end   <- as.integer(region_parts[3])

  start <- max(1, start - padding)
  end   <- end + padding
  region_expanded <- paste0(chrom, ":", start, "-", end)

  sample_bam <- file.path(bam_dir, paste0(sample, ".markdup.sorted.bam"))
  control_bams <- file.path(bam_dir, paste0(controls, ".markdup.sorted.bam"))
  bam_files <- c(sample_bam, control_bams)
  groups <- c("Patient", control_labels)
  samples <- c(sample, controls)

  sashimi_df <- data.frame(
    samples = samples,
    bam_files = bam_files,
    groups = groups,
    stringsAsFactors = FALSE
  )

  sashimi_input <- tempfile(fileext = ".tsv")
  on.exit(unlink(sashimi_input), add = TRUE)
  write.table(
    sashimi_df,
    file = sashimi_input,
    sep = "\t",
    quote = FALSE,
    row.names = FALSE,
    col.names = FALSE
  )

  output_pdf <- file.path(output_dir, paste0(sample, "_", region, "_sashimi_plot.pdf"))
  output_png <- sub("\\.pdf$", ".png", output_pdf)

  cmd <- paste(
    ggsashimi_path,
    "-b", sashimi_input,
    "-c", region_expanded,
    "-g", gtf_file,
    "-o", output_pdf,
    "-M 10 -C 3 -O 3 --alpha 0.25 -A median_j",
    "--base-size=20 --ann-height=3 --height=5 --width=18 --shrink --fix-y-scale"
  )

  system(cmd)

  # Convert to PNG
  img <- image_read_pdf(output_pdf, density = 150)
  image_write(img, output_png, format = "png")

  return(list(pdf = output_pdf, png = output_png))
}

#' Generate a faceted violin-boxplot of gene expression across samples
#'
#' @description
#' This function plots normalized expression values for one or more genes from an OUTRIDER dataset
#' across all samples. Each gene is displayed in a separate facet. A specified sample is highlighted
#' in red for easy comparison.
#'
#' @param ods An OUTRIDER dataset (SummarizedExperiment object)
#' @param genes A character vector of gene symbols or Ensembl IDs to plot
#' @param sampleID A character string specifying the sample ID to highlight
#'
#' @return A ggplot2 object showing expression distributions with violin and boxplots, faceted by gene
#' @export
#'
#' @importFrom dplyr filter
#' @importFrom tidyr gather
#' @importFrom ggplot2 ggplot geom_violin geom_boxplot geom_point facet_wrap theme element_blank labs aes
#' @importFrom SummarizedExperiment rownames
#' @importFrom OUTRIDER counts

plot_gene_expression_multi <- function(ods, genes, sampleID) {
  # Extract and reshape normalized counts
  counts_norm <- as.data.frame(counts(ods, normalized = TRUE))
  counts_norm$geneID <- rownames(counts_norm)

  long_counts <- counts_norm %>%
    tidyr::gather("sample_ID", "counts", -geneID) %>%
    dplyr::filter(geneID %in% genes)

  # Highlighted sample
  highlight <- dplyr::filter(long_counts, sample_ID == sampleID)
  other <- dplyr::filter(long_counts, sample_ID != sampleID)

  # Plot
  p <- ggplot(other, aes(x = factor(0), y = counts)) +
    facet_wrap(~geneID, scales = "free_y") +
    geom_violin(fill = "white") +
    geom_boxplot(width = 0.3, outlier.shape = NA) +
    #geom_jitter(color = "darkgrey", size = 1, alpha = 0.8) +
    geom_point(data = highlight, aes(x = factor(0), y = counts), color = "red", size = 2) +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.x = element_blank()
    ) +
    labs(
      x = NULL,
      y = "Expression (normalized counts)",
      title = paste("Expression for", paste(genes, collapse = ", "))
    )

  return(p)
}

