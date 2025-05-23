% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_data_processing.R
\name{fixFdsH5Paths}
\alias{fixFdsH5Paths}
\title{Fix the FRASER results directory}
\usage{
fixFdsH5Paths(fds, base_path, working_dir = "./data/new", verbose = FALSE)
}
\arguments{
\item{fds}{A FRASER dataset}

\item{base_path}{The path to the directory containing the `.h5` objects (default: "./data/savedObjects/MUSCLE--v38/")}

\item{working_dir}{The path to the working directory}
}
\value{
A modified object with appropriate h5 paths
}
\description{
Fix the FRASER results directory
}
