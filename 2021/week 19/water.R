library(tidytuesdayR)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(sf)
library(rnaturalearth)

# Get the data

tuesdata <- tidytuesdayR::tt_load(2021, week = 19)

water <- tuesdata$water

# Filter latin america countries 

raw_df <- read_csv("2021/week 19/Water_Point_Data_Exchange__WPDx-Basic_.csv")

clean_df <- raw_df %>% 
  janitor::clean_names() %>%
  rename_with(~str_remove(.x, "number_")) %>% 
  select(
    row_id:status_id, water_source_clean:subjective_quality,rehab_year,
    install_year,
    -contains("adm"), -contains("rehab"), -contains("fecal"),
    -count, -new_georeferenced_column, -source, -management,-subjective_quality
  ) %>% 
  mutate(
    status_id = case_when(
      status_id == "Yes"~ "y",
      status_id == "No" ~ "n",
      status_id == "Unknown" ~ "u"
    ),
    report_date = str_sub(report_date, 1, 10)
  ) %>% 
  filter(
    country_name %in% c(
      "Argentina", "Bolivia", "Brasil", "Chile", "Colombia", # the 20 countries of LA
      "Costa Rica", "Cuba", "Dominican Republic", "Ecuador", "Guatemala", "Nicaragua", 
      "El Salvador", "French Guiana", "Haiti", "Honduras", "Guadeloupe", "Martinique",
      "Dominican Republublic", "Mexico", "Panama", "Paraguay", "Peru", "Puerto Rico", 
      "Saint Barthélemy", "Uruguay", "Saint Martin", "Venezuela"
    )
  ) %>% 
  select(-water_tech) %>% 
  rename(water_source = water_source_clean, water_tech = water_tech_clean)

clean_df %>% 
  write_rds("2021/week 19/water_la.rds")

# Check the data

water_la <- read_rds("2021/week 19/water_la.rds")

lista <- water_la %>% 
  count(water_source, country_name) 


central_paises <- 
  ne_countries(country = c("El Salvador", "Mexico","Honduras", "Nicaragua"), 
               scale = "large", returnclass = "sf") 

central_estados <- 
  ne_states(iso_a2 = c("NI", "SV", "HN", "MX"), returnclass = "sf") 


