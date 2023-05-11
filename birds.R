#BIRDS ----
#packages ----
library(tidyverse)
library(lubridate)
library(tidyr)
library(dplyr)
library(readxl)
library(snakecase)
library(janitor)
#Import data ----

birds <- read_excel("Bird data/bird_banding_taricaya.xlsx", 
                    sheet = "RECAPTURES")
head(birds)

 

#_____________________________________ ----
#Clean data ----
#Clean coloumns ----
colnames(birds)

summary(birds)

glimpse(birds)

birds <- janitor::clean_names(birds)

#Translate coloumn names ----

birds <- rename(birds, "species"="nombre_especie",
                "code"="codigo",
                "tail"="cola",
                "beak"="pico_nar",
                "tarsus"="tarso",
                "weight"="peso",
                "wing"="ala",
                "station"="estacion",
                "day"="dia",
                "month"="mes",
                "year"="ano")
                

#refining data ----
new_birds <- birds %>%
  select(species,
         tail,
         beak,
         tarsus,
         weight,
         wing,
         station,
         day,
         month,
         year)

new_birds %>%
  distinct(species)

new_birds %>%
  duplicated %>%
  sum()

#Remove placeholder "-9" values ----
glimpse(new_birds)

new_birds <- new_birds %>%
  mutate(tail = na_if(tail, "-9"))
new_birds <- new_birds %>%
  mutate(beak = na_if(beak, "-9"))
new_birds <- new_birds %>%
  mutate(tarsus = na_if(tarsus, "-9"))
new_birds <- new_birds %>%
  mutate(weight = na_if(weight, "-9"))
new_birds <- new_birds %>%
  mutate(wing = na_if(wing, "-9"))

new_birds <- new_birds %>%
mutate(tail = na_if(tail, "NA"))
new_birds <- new_birds %>%
  mutate(beak = na_if(beak, "NA"))
new_birds <- new_birds %>%
  mutate(tarsus = na_if(tarsus, "NA"))
new_birds <- new_birds %>%
  mutate(weight = na_if(weight, "NA"))
new_birds <- new_birds %>%
  mutate(wing = na_if(wing, "NA"))

#drop na values from dataset ----
new_birds <- new_birds %>%
  na.omit(new_birds)

#Convert to numeric ----
glimpse(new_birds)


new_birds <- new_birds %>%
mutate(wing = as.numeric(wing))

new_birds <- new_birds %>%
  mutate(tail = as.numeric(tail))

new_birds <- new_birds %>%
  mutate(beak = as.numeric(beak))

new_birds <- new_birds %>%
  mutate(tarsus = as.numeric(tarsus))

new_birds <- new_birds %>%
  mutate(weight = as.numeric(weight))

new_birds <- new_birds %>%
  mutate(day = as.numeric(day))

glimpse(new_birds)


  
#_____________________________________ ----
#DATA VIZ ----

new_birds %>%
  ggplot(aes(x=weight,
             y=wing))+
  geom_point()+
  theme_classic()
  
  
