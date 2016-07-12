#' Find all geographies of a specified geography type..
#'
#' @description This function allows users to find all geographies of a specified geography type.
#' @param geography A character with type pf Geography. \code{county, censusplace, msa, usf, statesenate, statehouse, congdistrict, tribalnation}
#' @param geography A character with type pf Geography.
#' @return If \code{stateName="nation"} returns national bip
#' @examples
#' geolookup(geography = "state")

# http://www.broadbandmap.gov/developer/api/geography-lookup-api-by-geography-type
geolookup<-function(geography = "state", maxresults=100){

  if (is.null(geography) || is.null(maxresults))
    stop("The parameters geography or maxresults can not be NULL")

  if (is.na(geography) || is.na(maxresults))
    stop("The parameters geography or maxresults can not be NA")

  if (is.nan(geography) || is.nan(maxresults))
    stop("The parameters geography or maxresults can not be NAN")

  url<-paste0("http://www.broadbandmap.gov/broadbandmap/geography/",geography,"?format=json&maxresults=",maxresults)
  document <- fromJSON(txt=url)
  df<-as.data.frame(document$Results)
  return(df)
}

