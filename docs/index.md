# RNA Web App Documentation

_A comprehensive guide to using the RNA Transcriptomics Analysis App_

_Last updated: 2025-06-26_

---

## 📌 Overview

Welcome to the **RNA Transcriptomics Web App**!  
This application is designed to support the analysis of:

- Differential gene expression
- Splicing outliers (FRASER, OUTRIDER)
- Gene expression per sample
- RNA fusion and variant detection

---

## 🚀 Getting Started

### 1. Upload Required Files

In the **Upload** tab, you must provide:

- ✅ A `samplesheet` file (CSV or TSV) with RNA_IDs and metadata  
- 🔁 Optionally:
  - OUTRIDER results
  - FRASER results
  - RNA variant calls (VCF)
  - RNA fusion calls (Arriba, STAR-Fusion, etc.)

> ⚠️ Sample IDs must match across all files.

---

### 2. Cohort Analysis

Navigate to the **Cohort Results** tab to explore patterns across multiple samples.

- Select a phenotype (via PanelApp)
- Optionally select specific genes
- Choose how many gene panels to show at once
- Click **Display Results**
- Page through gene plots, and inspect FRASER/OUTRIDER results

You can export tables or plots from this tab.

---

### 3. Individual Sample Results

In the **Individual Results** tab:

- Select a sample using the dropdown
- Click **Display Results** to view:
  - Overlapping results (FRASER + OUTRIDER)
  - Volcano plots
  - Detailed result tables
  - Sashimi plots of splicing events
  - Variant/fusion data if uploaded

---

## 📊 Sashimi Plotting

To generate a Sashimi plot for a splicing event:

1. Select a sample and click **Display Results**
2. Choose an aberrant event from the dropdown
3. Select at least two control samples
4. Click **Generate Sashimi Plot**

---

## 📦 Downloading Results

The app allows you to download:

- 🧾 **Excel files** with FRASER and OUTRIDER results
- 📄 **PDF reports** with:
  - Selected sample metadata
  - Summary plots
  - Sashimi plots
  - Filtered result tables

Click the **Download Report** or **Download Excel** button on the relevant tab.

---

## 💡 Tips & Troubleshooting

- Make sure all files have matching sample IDs
- If no results appear, check that:
  - At least one gene or phenotype was selected
  - The sample has splicing/DE events
- You must click **Display Results** to trigger any output
- Use Chrome or Firefox for best experience

---

## 🛠 File Requirements

### Sample Sheet (CSV/TSV)

| RNA_ID | PHENOTYPE | ... |
|--------|-----------|-----|
| S01    | case      | ... |
| S02    | control   | ... |

### Result File Naming

Ensure file naming follows expected patterns or matches what's required by the parser.

---

## 🧬 Tools Used

This app integrates:

- [FRASER](https://bioconductor.org/packages/release/bioc/html/FRASER.html)
- [OUTRIDER](https://bioconductor.org/packages/release/bioc/html/OUTRIDER.html)
- [PanelApp](https://panelapp.genomicsengland.co.uk/)
- [Arriba](https://github.com/suhrig/arriba)
- [STAR-Fusion](https://github.com/STAR-Fusion/STAR-Fusion)

---

## 📞 Contact

If you have questions or need support:

- 📧 Email: `chiara.folland@perkins.org.au`
- 🧪 GitHub: [ChiaraF32/rnawebapp](https://github.com/ChiaraF32/rnawebapp)

---

_Thank you for using the RNA Transcriptomics Web App!_
