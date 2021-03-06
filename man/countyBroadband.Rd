% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/countyBroadband.R
\name{countyBroadband}
\alias{countyBroadband}
\title{Search broadband availability by county name.}
\usage{
countyBroadband(version = "jun2014", county = NULL)
}
\arguments{
\item{county}{A character with a minimum of 1 character.}

\item{date}{A character with data version. Examples: jun2014, dec2013, jun2013, dec2012, jun2012, dec2011}
}
\value{
A data.frame with these variables: stateName countyName availabilityGt50PercentFlag countyId stateId
}
\description{
The api allows you to search by county name for list of counties with response having a flag for fixed broadband availability greater than 50% for each county.
}
\examples{
countyBroadband(county="Miami")
}

