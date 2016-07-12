#' This function allows the user to search for the demographic information by ID.
#'
#' @description This function allows the user to search for the demographic information for a particular geography type and geography ID from http://www.broadbandmap.gov/.
#' @param version A character with data version. Examples: jun2014, dec2013, jun2013, dec2012, jun2012, dec2011.
#' @param geography A character with geography type. It can be censusplace, msa, usf, county, statesenate, statehouse, congdistrict.
#' @param geographyId A character or vector of characteres with geographicals ID.
#' @return A dataframe with following information: http://www.broadbandmap.gov/broadbandmap/api/dictionary/v1/demographics?format=json
#' @examples
#' demographicsByGeo(version = "jun2014", geography = "county",
#' geographyId = c("01001", "01003", "01005", "01007", "01011", "010015", "01017", "01013", "01025", "01035", "01023")

# http://www.broadbandmap.gov/developer/api/demographics-api-by-geography-type-and-geography-id
demographicsByGeo<-function(version="jun2014", geography=NULL, geographyId=NULL){

  if (is.null(version) || is.null(geography) || is.null(geographyId))
    stop("The parameters version, geography and geographyId can not be NULL")

  if (is.na(version) || is.na(geography) || is.na(geographyId))
    stop("The parameters version, geography and geographyId can not be NA")

  if (is.nan(version) || is.nan(geography) || is.nan(geographyId))
    stop("The parameters version, geography and geographyId can not be NAN")

  lista<-split(1:length(geographyId), ceiling(seq_along(1:length(geographyId))/10))

  lista<-lapply(X=lista, function(x, geographyId){
    geographyId[x]
  }, geographyId=geographyId)

  lista<-lapply(X=lista, function(x){
    ids<-paste0(x, collapse = ",")
    url<-paste0("http://www.broadbandmap.gov/broadbandmap/demographic/",version,"/",geography,"/ids/",ids,"?format=json")
    document <- fromJSON(txt=url)
    as.data.frame(document$Results)
  })

  df<-as.data.frame(lista)
  return(df)
}


