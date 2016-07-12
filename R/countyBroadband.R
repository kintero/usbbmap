#' Search broadband availability by county name.
#'
#' @description The api allows you to search by county name for list of counties with response having a flag for fixed broadband availability greater than 50% for each county.
#' @param county A character with a minimum of 1 character.
#' @param date A character with data version. Examples: jun2014, dec2013, jun2013, dec2012, jun2012, dec2011
#' @return A data.frame with these variables: stateName countyName availabilityGt50PercentFlag countyId stateId
#' @examples
#' countyBroadband(county="Miami")

# http://www.broadbandmap.gov/developer/api/county-broadband-availability-api-search-by-county-name
countyBroadband<-function(version="jun2014", county=NULL){

  if (is.null(county) || is.na(county)|| is.nan(county)){
    stop("county can not be NULL/NA/NAN")
  }

  if (is.null(version) || is.na(vesion)|| is.nan(version)){
    stop("county can not be NULL/NA/NAN")
  }

  url<-paste0("http://www.broadbandmap.gov/broadbandmap/county-availability/",version,"/county/", county, "?format=json")
  document <- fromJSON(txt=url)
  df<-as.data.frame(document$Results)
  return(df)
}

