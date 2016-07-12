#' This function finds all the geographies specified by geography name.
#'
#' @description This function finds all the geographies specified by a geography name (e.g., Washington) of a specific geography type (e.g., congressional district) within the entire United States.
#' @param geography A character with geography type. It can be state, county, tract, block, congdistrict, statehouse, statesenate, censusplace or msa.
#' @param name A character with at least 3 leading characters.
#' @param maxresults specify the maximum results to be returned - defaults to 100.
#' @return A dataframe with following information: http://www.broadbandmap.gov/broadbandmap/api/dictionary/v1/census?format=json
#' @examples
#' censusByGeo(geography="county", name="Miami", maxresults=5)


# http://www.broadbandmap.gov/developer/api/census-api-by-geography-name
censusByGeo<-function(geography=NULL, name=NULL, maxresults=5){

  if (is.null(geography) || is.null(name) || is.null(maxresults))
    stop("The parameters geography and geographyId can not be NULL")

  if (is.na(geography) || is.na(name) || is.na(maxresults))
    stop("The parameters geography and geographyId can not be NA")

  if (is.nan(geography) || is.nan(name) || is.nan(maxresults))
    stop("The parameters geography and geographyId can not be NAN")

  if (!(geography %in% c("state", "county", "tract", "block", "congdistrict", "statehouse", "statesenate", "censusplace", "msa")))
    stop("The geography options avaibable are state, county, tract, block, congdistrict, statehouse, statesenate, censusplace or msa")

  url<-paste0("http://www.broadbandmap.gov/broadbandmap/census/", geography, "/",name,"?maxresults=",maxresults ,"&format=json")
  document <- fromJSON(txt=url)
  df<-as.data.frame(document$Results)
  return(df)
}


