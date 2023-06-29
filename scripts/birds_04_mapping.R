#Mapping the birds of taricaya

#Packages ----
library(tidyverse)
library(highcharter)
library(sp)
library(rgdal)
### Map visualisation ----
world_map <- map_data("world")

world_map %>%
ggplot(aes(x=long,
           y=lat,
           group=group))+
geom_path()+
  xlim(-82,-68)+
  ylim(-18.5,0)+
           theme_minimal()

peru_map <- map_data("world") %>%
  filter(region == "Peru")


peru_map %>%
  hchart(aes(x=long,
             y=lat))+
  geom_polygon()

number_of_birds <- all_birds %>%
  group_by(species)%>%
          summarise(n=n())

hcmap("countries/peru")







hc <- highchart() %>%
  hc_add_series_map(
    worldgeojson, life.exp, value = "value", joinBy = c('name','country'),
    name = "LifeExpectancy"
  ) 
 hc
 
### Reading coordinates ----

#converting MGRS to lat/long
 
 site_code <- c("CAWW",
               "PANC",
               "AMAZ",
               "OSOB",
               "SWAM",
               "EUGE",
               "LEON",
               "HOBO",
               "NAVI",
               "LAGU",
               "ISLA",
               "NEFA",
               "NEF2",
               "RIO2",
               "TARI")
 
 UTM_zone <- c("19L",
           "19L",
           "19L",
           "19L",
           "19L",
           "19L",
           "19L",
           "19L",
           "19L",
           "19L",
           "19L",
           "19L",
           "19L",
           "19L",
           "19L")
 
 UTM_easting <- c("502115",
                  "502207",
                  "502653",
                  "501682",
                  "501760",
                  "501283",
                  "501690",
                  "502081",
                  "500095",
                  "500782",
                  "501060",
                  "501343",
                  "503359",
                  "502591",
                  "502174")
 
 UTM_northing <- c("8615299",
                   "8615188",
                   "8615287",
                   "8615023",
                   "8614654",
                   "8614050",
                   "8613747",
                   "8615455",
                   "8615332",
                   "8615897",
                   "8616083",
                   "8616064",
                   "8615456",
                   "8615959",
                   "8616098")
 
 #Create data frame with site name and UTM coords
UTM_data <- data.frame(site_code, UTM_zone, UTM_easting, UTM_northing)

#Convert to numeric
UTM_data$UTM_easting <-  as.numeric(UTM_data$UTM_easting)
UTM_data$UTM_northing <-  as.numeric(UTM_data$UTM_northing)
 
#Create a spatial object using UTM coords
data_sp <- SpatialPoints(UTM_data[, c("UTM_easting", "UTM_northing")], proj4string = CRS("+proj=utm +zone=19 +datum=WGS84 +units=m +no_defs"))

 
#Convert spatial object to lat/long
data_ll <- spTransform(data_sp, CRS("+proj=longlat +datum=WGS84"))


lat_long <- coordinates(data_ll)

 
###Plotting site location on map ----


# Convert the matrix to a data frame
my_data <- as.data.frame(lat_long)

# Use fortify with the converted data frame
UTM_plot_data <- fortify(my_data)

#add group variable
UTM_plot_data$group <- 1124


#Get map
world_map <- map_data("world")

world_map %>%
  ggplot(aes(x = long,
             y = lat,
             group = group))+
  geom_path()+
  geom_point(data = UTM_plot_data, aes(x = UTM_easting,
                                        y = UTM_northing),
             colour= "red") +
  xlim(-82,-68)+
  ylim(-18.5,0)+
  theme_minimal()
