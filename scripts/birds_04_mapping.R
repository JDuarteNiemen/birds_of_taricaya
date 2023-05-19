#Mapping the birds of taricaya

#Packages ----
library(tidyverse)
library(highcharter)

peru_map <- map_data("world") %>%
  filter(region == "Peru")


peru_map %>%
  hchart(aes(x=long,
             y=lat))+
  geom_polygon()



hcmap("countries/peru")







hc <- highchart() %>%
  hc_add_series_map(
    worldgeojson, life.exp, value = "value", joinBy = c('name','country'),
    name = "LifeExpectancy"
  ) 
hc