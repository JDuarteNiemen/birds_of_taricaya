#BIRDS ----
#packages ----
library(tidyverse)
library(lubridate)
library(tidyr)
library(dplyr)
library(readxl)
library(snakecase)
library(janitor)

#______________________________________________________________---- 
  
clean_excel_sheets <- function(file_path, sheet_names) {
  
  # Read in each sheet, clean the data, and store it in a list
  sheet_list <- lapply(sheet_names, function(sheet_name) {
    
    # Read in the sheet
    sheet <- read_excel(file_path, sheet = sheet_name)
    
    # Clean the column names
    sheet <- janitor::clean_names(sheet)
    sheet <- rename(sheet, "species"="nombre_especie",
                    "code"="codigo",
                    "tail"="cola",
                    "beak"="pico_nar",
                    "tarsus"="tarso",
                    "weight"="peso",
                    "wing"="ala",
                    "station"="estacion",
                    "day"="dia",
                    "month"="mes",
                    "year"="ano",
                    "sex"="sexo")
    
    # Select relevant columns
    sheet <- select(sheet,
                    species,
                    tail,
                    beak,
                    tarsus,
                    weight,
                    wing,
                    station,
                    day,
                    month,
                    year,
                    sex)
    
   

    # Convert relevant columns to numeric
    sheet <- sheet %>%
      mutate(across(c(wing, tail, beak, tarsus, weight, day, year, month), as.numeric))
    
    
    # Remove placeholder "-9" and "NA" values
    sheet <- sheet %>%
      mutate(across(c(tail, beak, tarsus, weight, wing, station, species, day, month, year, sex ), ~ ifelse(.x == "-9" | .x == "NA", NA,
                    ifelse((.col == "day" & as.numeric(.x) > 31) | (.col == "month" & as.numeric(.x) > 12), NA, .x))))
    
   
    
    # Return the cleaned sheet
    return(sheet)
  })
  
  # Return the list of cleaned sheets
  return(sheet_list)
}


file_path <- "Bird data/bird_banding_taricaya.xlsx"
sheet_names <- c("A", "B", "C", "D", "E", "F", "All","2009", "RAMCAR", "RECAPTURES")
cleaned_sheets <- clean_excel_sheets(file_path, sheet_names) 

birds_a <- cleaned_sheets[[1]]
birds_b <- cleaned_sheets[[2]]  
birds_c <- cleaned_sheets[[3]]
birds_d <- cleaned_sheets[[4]]
birds_e <- cleaned_sheets[[5]]
birds_f <- cleaned_sheets[[6]]
birds_g <- cleaned_sheets[[7]]
birds_h <- cleaned_sheets[[8]]
birds_i <- cleaned_sheets[[9]]
birds_j <- cleaned_sheets[[10]]

#merge dataframes

all_birds <- bind_rows(birds_a, birds_b, birds_c, birds_d, birds_e, birds_f, birds_g, birds_h, birds_i, birds_j)


#further cleaning ----

all_birds <- all_birds %>%
  mutate(month = if_else(month > 12, NA_integer_, month))%>%
  mutate(day = if_else(day > 31, NA_integer_, day))#replacing impossible values for day and month to NA

glimpse(all_birds)


all_birds %>%
  distinct(sex)
