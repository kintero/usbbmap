% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/osmGeocoder.R
\name{osmGeocoder}
\alias{osmGeocoder}
\title{OSM Geocoder}
\usage{
osmGeocoder(location)
}
\arguments{
\item{location}{a character string or list specifying a location of interest.}
}
\value{
data.frame with variables lon and lat
}
\description{
This function use Nominatim API to getting coordinates from address. I decided to add it because sometimes coordinates are not avaibable.
}
\details{
geocodes a location using Nominatim API.
}
\examples{
osmGeocoder("4800 W. Copans Road, Coconut Creek, FL 33063")
}
\seealso{
\url{http://wiki.openstreetmap.org/wiki/Nominatim}
}

