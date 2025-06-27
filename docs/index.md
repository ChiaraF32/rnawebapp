# RNA Web App Documentation

_A comprehensive guide to using the RNA Transcriptomics Analysis App_

_Last updated: 2025-06-27_

---

## 📌 Overview

Welcome to the **RNA Transcriptomics Web App**! 
This app is designed to simplify the analysis of RNAseq data from cohorts of rare disease patients, including:

- Gene expression outliers (OUTRIDER)
- Splicing outliers (FRASER)
- RNA variant calling (GATK HaplotypeCaller)
- RNA fusions and structural variant detection (Arriba, STAR-Fusion)


## Prerequisites 

To use the app, you first need to run the [Detection of RNA Outliers Pipeline (DROP)](https://gagneurlab-drop.readthedocs.io/en/latest/).
DROP consists of four modules designed for each of the following analyses:

1. Aberrant Expression (OUTRIDER)
2. Aberrant Splicing (FRASER)
3. Monoallelic Expression 
4. RNA Variant Calling

If you want to include RNA fusion and structural variant analyses, you must run [RNAvc](https://github.com/RAVING-Informatics/rnavc_snakemake/),
a snakemake workflow which runs Star-Fusion and Arriba and combines the outputs into a meaningful summary.

---

## 💡 Tips & Troubleshooting

- Make sure all files have matching sample IDs
- If no results appear, check that:
  - At least one gene or phenotype was selected
  - The sample has splicing/DE events
- You must click **Display Results** to trigger any output
- Use Chrome or Firefox for best experience

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
