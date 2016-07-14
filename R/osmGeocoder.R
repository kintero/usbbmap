#' OSM Geocoder
#'
#' geocodes a location using Nominatim API.
#'
#' @description This function use Nominatim API to getting coordinates from address. I decided to add it because sometimes coordinates are not avaibable.
#' @param location a character string or list specifying a location of interest.
#' @return data.frame with variables lon and lat
#' @seealso \url{http://wiki.openstreetmap.org/wiki/Nominatim}
#' @examples
#' osmGeocoder("4800 W. Copans Road, Coconut Creek, FL 33063")

osmGeocoder <- function(location) {
  location<-gsub(' ', "+", location)
  url_string <- paste0('http://nominatim.openstreetmap.org/search?format=json&addressdetails=0&limit=1&q=', location)

  list<-sapply(url_string, fromJSON)
  lat<-NULL
  lon<-NULL

  if (length(url_string)>1){

    # Get Latitude
    lat<-lapply(list, '[[', "lat")
    lat<-lapply(lat, FUN = function(x){
      ifelse(is.null(x), NA, x)
    })
    lat<-unlist(lat)

    # Get Longitude
    lon<-lapply(list, '[[', "lon")
    lon<-lapply(lon, FUN = function(x){
      ifelse(is.null(x), NA, x)
    })
    lon<-unlist(lon)}else{

      names(list)<-attr(list, "dimnames")[[1]]

      # Get Latitude
      lat<-list$lat
      lat<-lapply(lat, FUN = function(x){
        ifelse(is.null(x), NA, x)
      })
      lat<-unlist(lat)

      # Get Longitude
      lon<-list$lon
      lon<-lapply(lon, FUN = function(x){
        ifelse(is.null(x), NA, x)
      })
      lon<-unlist(lon)
    }

  df<-data.frame(lat=as.numeric(as.character(lat)), lon=as.numeric(as.character(lon)))
  rownames(df)<-NULL
  return(df)
}
