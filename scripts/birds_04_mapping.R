#Mapping the birds of taricaya

#Packages ----
library(tidyverse)
library(highcharter)


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