# 🚀 App Usage

## 1. Launch App
- See the Installation instructions to create an instance of the app.
- Once launched, select **Get Started** to navigate to the file upload.

---

## 2. Upload Required Files

In the **Upload** tab, you must provide:

- ✅ A `samplesheet` file (CSV or TSV) with RNA_IDs and metadata  
- 🔁 Optionally:
  - OUTRIDER dataset
  - FRASER dataset
  - RNA variant calls (VCF)

Once the data is uploded, click the **Data Processing** to process the uploaded data. 

---

## 3. Data Processing

In the **Data Processing** tab, you must select an appropriate p-adjust value threshold to use to calculate the OUTRIDER and FRASER results.
Once selected, click **Process Data** to launch the data processing step. 
This step will take approximately 2 minutes, but may run for longer if you have a very large dataset (>150 samples).
Once the data has been processed, a summary of the cohort will be generated, including:
  - A sample result summary, detailing which analyses are available for each sample
  - A bar graph of the phenotypes in the cohort
  - The number of expression and splicing outliers calculated per sample, and the average across the cohort
  - The number of RNA variants in each sample

Once the data processing is complete, select **Parameter Selection** to choose how you would like to further analyse the data.
  
---

## 4. Parameter Selection 

In this tab, you can select either **Cohort Analysis** or **Sample Analysis**

You can also select which results you want to view by checking / unchecking the following:

  - Aberrant expression
  - Aberrant splicing
  - RNA variants
  - RNA fusions and SVs

## 5. Results

### Cohort Results 

This analysis is designed to view the results of the cohort of a whole.
It is useful if you are interested in outliers in specific genes (e.g. specific to a phenotype of interest)

1. Select genes of interest by either:
    - Choosing a phenotype (via PanelApp), and/or
    - Manually selecting genes of interest
    *Note: You can only select genes that are expressed in the dataset*
2. Choose how many gene plots to show in each panel at once
3. Select a sample of interest (optional) 
4. Click **Display Results**
5. Page through gene plots, and inspect FRASER/OUTRIDER results filtered for the gene(s) of interest

You can optionally export tables or plots from this tab by selecting

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
