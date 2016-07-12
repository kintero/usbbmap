#' This function returns broadband summary data by geography IDs for a specific geography type.
#'
#' @description It is designed to retrieve broadband summary data by geography and census metrics (population or households) combined as search criteria. The data includes wireline and wireless providers, different technologies and broadband speeds reported in the particular area being searched for on a scale of 0 to 1.
#' @param date A character with data version. Examples: jun2014, dec2013, jun2013, dec2012, jun2012, dec2011.
#' @param censusMetrics A character with census metric. It can be either \code{population} or \code{household}.
#' @param geography A character with geography type. It can be state, censusplace, msa, county, statesenate, statehouse, congdistrict, usf, tribalnation or nation. If nation is choosen, it will return NATIONAL information.
#' @param geographyId A character or vector of character with the corresponding geographical ID. If geographic is \code{nation}, this parameter will not be used.
#' @return A dataframe with following information by geography: http://www.broadbandmap.gov/broadbandmap/api/dictionary/v1/broadbandsummary?format=json
#' @examples
#' summaryBroadband(date="jun2014", censusMetrics="population", geography="state", geographyId=c(22,41))

# http://www.broadbandmap.gov/developer/api/broadband-summary-api-by-geography-type-and-geography-id
summaryBroadband<-function(version="jun2014", censusMetrics=NULL, geography=NULL, geographyId=NULL){

  if (is.null(version) || is.null(censusMetrics) || is.null(geography))
    stop("The parameters version, censusMetrics and geography can not be NULL")

  if (!(version %in% c("jun2014", "dec2013", "jun2013", "dec2012", "jun2012", "dec2011")))
     stop("The date options avaibable are jun2014, dec2013, jun2013, dec2012, jun2012, dec2011")

  if (!(censusMetrics %in% c("population", "household")))
    stop("The censusMetrics options avaibable are population or household")

  if (!(geography %in% c("state", "censusplace", "msa", "county", "statesenate", "statehouse", "congdistrict", "usf", "tribalnation", "nation")))
    stop("The geography options avaibable are state, censusplace, msa, county, statesenate, statehouse, congdistrict, usf, tribalnation or nation")

  geographyId=paste0(geographyId, collapse = ",")
  if (geography=="nation"){
    url<-paste0("http://www.broadbandmap.gov/broadbandmap/analyze/", version, "/summary/", censusMetrics,"/", geography,"?format=json")
  }else{
    url<-paste0("http://www.broadbandmap.gov/broadbandmap/analyze/", version, "/summary/", censusMetrics, "/", geography,"/ids/", geographyId, "?format=json")
  }
  document <- fromJSON(txt=url)
  df<-as.data.frame(document$Results)
  return(df)
}

# ### Information API - Review Data Dictionary ####
# url<-paste0("http://www.broadbandmap.gov/broadbandmap/api/dictionary/v1/broadbandsummary?format=json")
# document <- fromJSON(txt=url)
# df<-as.data.frame(document$Results)

