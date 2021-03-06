% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eq_functions.R
\name{eq_clean_data}
\alias{eq_clean_data}
\title{Clean data}
\usage{
eq_clean_data(raw)
}
\arguments{
\item{raw}{A data frame or tibble with the raw data.
The country must be separated from the location name by a colon.}
}
\value{
A data frame or tibble with the cleaned data.
}
\description{
This function unites temporal information in a DATE column,
converts the spatial coordinates (LONGITUDE, LATITUDE),
the earthquake magnitude (EQ_PRIMARY),
and the number of deaths (TOTAL_DEATHS) to numeric,
cleans the LOCATION_NAME by using the \code{eq_location_clean} function,
and discards several columns not needed for the analysis of earthquakes in this package.
}
\examples{
file <- system.file("extdata", "earthquakes.tsv.gz", package = "NOAAeq")
data <- readr::read_delim(file = file, delim = "\\t")
eq_clean_data(data)

}
