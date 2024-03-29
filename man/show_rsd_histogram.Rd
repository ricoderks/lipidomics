% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/show_rsd_histogram.R
\name{show_rsd_histogram}
\alias{show_rsd_histogram}
\title{Show the histogram of all RSD values}
\usage{
show_rsd_histogram(qc_data, rsd)
}
\arguments{
\item{qc_data}{tibble in tidy format}

\item{rsd}{rsd threshold to show in the plot}
}
\value{
ggplot2 object
}
\description{
Show the histogram of all RSD values of all lipids within
    all QCpool samples..
}
\author{
Rico Derks
}
