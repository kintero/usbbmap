
# Instalation.

**Development version**

```{r}
install.packages("devtools")
devtools::install_github("kintero/usbbmap")
```
# Get coordinates using address.

### Access to OSM/Nominatim API.

```{r}
address<-read.csv("data/address.csv")
coordinates<-osmGeocoder(address$x)
```

# Get demographics data by coodinates.

```{r}
data<-demographicsByCoord(lat = coordinates$lat, 
                           lon = coordinates$lon, 
                           year = "2014")

```




