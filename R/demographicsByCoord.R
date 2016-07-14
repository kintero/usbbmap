#' Get demographics information by coordinates.
#'
#' @description Find the demographics data from the coordinates.
#' @export
#' @param lat A numeric value, latitude
#' @param lon A numeric value, longitude
#' @param year A numeric value with 4 digits. Year of data. By the default 2014. The available years are 2014, 2013, 2012, 2011.
#' @return A data.frame with these variables: blockFips, incomeBelowPoverty, medianIncome, incomeLessThan25, incomeBetween25to50, incomeBetween50to100, incomeBetween100to200, incomeGreater200, educationHighSchoolGraduate, educationBachelorOrGreater.
#' @examples
#' demographicsByCoord(lat=25.7766074, lon = -80.3714869, year=2014)

# http://www.broadbandmap.gov/developer/api/demographics-api-by-coordinates

demographicsByCoord<-function(lat=NULL, lon=NULL, year=2014){

  if (is.null(lat) || is.null(lon) || is.null(year))
    stop("The parameters lat, lon and year can not be NULL")

  if (is.na(lat) || is.na(lon) || is.na(year))
    stop("The parameters lat, lon and year can not be NA")

  if (is.nan(lat) || is.nan(lon) || is.nan(year))
    stop("The parameters lat, lon and year can not be NAN")

  data<-as.data.frame(cbind(lat, lon, year))
  data$url<-with(data, paste0("http://www.broadbandmap.gov/broadbandmap/demographic/", year, "/coordinates?latitude=", lat, "&longitude=",lon,"&format=json"))

  document<-lapply(X=data$url, function(x){
    x<-fromJSON(txt=x)
    x$Results
  })

  df<-do.call(rbind.data.frame, document)
  return(df)
}



