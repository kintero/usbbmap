#' Get BIP (Broadband Initiatives Program) by State Ids.
#'
#' @description This function allows the user to find the BIP funding allocated to states by specifying the state fips code.
#' @param stateId A character or vector of characters with FIPS State Numeric Code. http://www.census.gov/geo/reference/ansi_statetables.html
#' @return A dataframe with following information: http://www.broadbandmap.gov/broadbandmap/api/dictionary/v1/bip?format=json.
#' @examples
#' bipByStateId(c("12","18","21"))

bipByStateId<-function(stateId=NULL){

  if (is.null(stateId))
    stop("The function requires valid state fips")

  lista<-split(1:length(stateId), ceiling(seq_along(1:length(stateId))/10))

  lista<-lapply(X=lista, function(x, stateId){
    stateId[x]
  }, stateId=stateId)

  lista<-lapply(X=lista, function(x){
    stateId<-paste0(x, collapse = ",")
    url<-paste0("http://www.broadbandmap.gov/broadbandmap/bip/stateids/", stateId,"?format=json")
    document <- fromJSON(txt=url)
    as.data.frame(document$Results)
  })

  df<-as.data.frame(lista)
  return(df)

}

# http://www.broadbandmap.gov/broadbandmap/api/dictionary/v1/bip?format=json
# http://www.broadbandmap.gov/developer/api/bip-funding-api-by-state-id

