#' This function finds the geography of a specified geography type by geography id.
#'
#' @description This API finds the geography of a specified geography type by geography id within the entire United States. Latest data version released jun2014. Previous data versions include jun2011, dec2011, jun2012, dec2012, jun2013, dec2013.
#' @param geography A character with geography type. It can be state, county, tract, block, congdistrict, statehouse, statesenate, censusplace, msa, tribal, usf.
#' @param geographyId A character or vector of character with the corresponding geographical ID. More information:
#' @return A dataframe with following information: http://www.broadbandmap.gov/broadbandmap/api/dictionary/v1/census?format=json
#' @examples
#' censusByFips(geography="state", geographyId="12")

# http://www.broadbandmap.gov/developer/api/census-api-by-fips-code
censusByFips<-function(geography="state", geographyId="01"){

  if (is.null(geography) || is.null(geographyId))
    stop("The parameters geography and geographyId can not be NULL")

  if (is.na(geography) || is.na(geographyId))
    stop("The parameters geography and geographyId can not be NA")

  if (is.nan(geography) || is.nan(geographyId))
    stop("The parameters geography and geographyId can not be NAN")

  url<-paste0("http://www.broadbandmap.gov/broadbandmap/census/", geography, "/fips/", geographyId,"?format=json")
  document <- fromJSON(txt=url)
  df<-as.data.frame(document$Results)
  return(df)
}
