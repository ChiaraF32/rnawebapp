% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_data_processing.R
\name{summarise_data}
\alias{summarise_data}
\title{Generate a summary table of aberrant genes detected per sample}
\usage{
summarise_data(ods, fds)
}
\arguments{
\item{ods}{An OUTRIDER dataset object.}

\item{fds}{A FRASER dataset object.}
}
\value{
A data.frame with one row per sample and the number of aberrant genes
detected by OUTRIDER (`OUTRIDER_outliers`) and FRASER (`FRASER_outliers`).
Samples missing in a dataset will have 0 as their outlier count.
}
\description{
This function returns a data frame summarizing the number of aberrant genes
detected in each sample by the OUTRIDER and FRASER datasets.
If a sample is not present in one of the datasets, it is assigned a count of 0.
}
