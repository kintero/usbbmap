#' Get census information by coordinates.
#'
#' @description This function returns the US Census Block geography ID information given a passed Latitude and Longitude. Latest data version released jun2014. Previous data versions include jun2011, dec2011, jun2012, dec2012, jun2013, dec2013.
#' @param lat A numeric value, latitude
#' @param lon A numeric value, longitude
#' @param geographyType A character. Valid geography types: state, county, tract, block, congdistrict, statehouse, statesenate, censusplace, msa.
#' @return A dataframe with following information: http://www.broadbandmap.gov/broadbandmap/api/dictionary/v1/census?format=json
#' @examples
#' censusByCoord(lat=25.7766074, lon = -80.3714869, geographyType="block")

censusByCoord<-function(lat=NULL, lon = NULL, geographyType=NULL){

  if (is.null(lat) || is.null(lon) || is.null(geographyType))
    stop("The parameters lat, long and geographyType can not be NULL")

  if (is.na(lat) || is.na(lon) || is.na(geographyType))
    stop("The parameters lat, lon and geographyType can not be NA")

  if (is.nan(lat) || is.nan(lon) || is.nan(geographyType))
    stop("The parameters lat, lon and geographyType can not be NAN")

  url<-paste0("http://www.broadbandmap.gov/broadbandmap/census/", geographyType, "?latitude=", lat, "&longitude=",lon,"&format=json")
  document <- fromJSON(txt=url)
  df<-as.data.frame(document$Results)
  return(df)
}


# ### Information API - Review Data Dictionary ####
# url<-paste0("http://www.broadbandmap.gov/broadbandmap/api/dictionary/v1/census?format=json")
# document <- fromJSON(txt=url)
# df<-as.data.frame(document$Results)

