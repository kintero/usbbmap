#' Get BIP (Broadband Initiatives Program) by State Names.
#'
#' @param stateName A character or vector of characters with names of States.
#' @return If \code{stateName="nation"} returns national bip
#' @examples
#' bipByStateName(stateName="florida")
#' bipByStateName(stateName=c("South Carolina", "florida"))
#' bipByStateName(stateName="nation")

# http://www.broadbandmap.gov/developer/api/bip-funding-api-by-state-name
bipByStateName<-function(stateName=NULL){

  if (is.null(stateName))
    stop("The function requires valid name")

  if (length(stateName)==1 && stateName=="nation"){
    url<-"http://www.broadbandmap.gov/broadbandmap/bip/nation?format=json"
    document <- fromJSON(txt=url)
    df<-as.data.frame(document$Results)
    return(df)
  }else{

    st<-paste(stateName, collapse = ",")
    if (sapply(gregexpr("\\W+", st), length) > 0){
      st<-gsub(" ", "%20", st)
    }
    url<-paste0("http://www.broadbandmap.gov/broadbandmap/bip/states/", st,"?format=json")
    document <- fromJSON(txt=url)
    df<-as.data.frame(document$Results)
    return(df)
  }
}
