% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_data_processing.R
\name{filter_samplesheet}
\alias{filter_samplesheet}
\title{Filter samplesheet to include only samples found in OUTRIDER or FRASER datasets}
\usage{
filter_samplesheet(samplesheet, ods, fds)
}
\arguments{
\item{samplesheet}{A data.frame containing a `RNA_ID` column}

\item{ods}{OUTRIDER dataset object}

\item{fds}{FRASER dataset object}
}
\value{
Filtered samplesheet including only samples present in either ods or fds
}
\description{
Filter samplesheet to include only samples found in OUTRIDER or FRASER datasets
}
