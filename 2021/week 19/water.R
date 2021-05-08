library(tidytuesdayR)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(sf)
library(gganimate)
library(rnaturalearth)

# Get the data and filter latin america countries 


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

water <- read_rds("2021/week 19/water_la.rds") %>% 
  filter(country_name == c("El Salvador","Honduras", "Nicaragua"))   #selected countries from latin america

list <- water %>% 
  group_by(country_name, water_tech, install_year) %>%
  summarise(n = n())

# Get the map data 

countries <- 
  ne_countries(country = c("El Salvador","Honduras", "Nicaragua"), 
               scale = "large", returnclass = "sf") 

# Edit water tech and coordinates

water_data <- water %>% 
  st_as_sf(coords = c("lon_deg", "lat_deg"), remove = FALSE) %>%    # Convert to sf object
  st_set_crs(st_crs(countries)) %>% # Set to same CRS
  filter(install_year>=1970) %>% # remove few observations from before %>% 
  filter(lon_deg < 1) %>% # remove 4 entries with errors in Nicaragua
  mutate(water_tech = replace(water_tech, str_detect(water_tech, "Hand Pump"), "Hand Pump")) %>% 
  mutate(water_tech = replace(water_tech, str_detect(water_tech, "Mechanized Pump"), "Mechanized Pump"))

water_data$water_tech[is.na(water_data$water_tech)] <- "Not identified"


# Plot

pal <- wesanderson::wes_palette("GrandBudapest1")

p <- ggplot(countries) +
  geom_sf(fill = "lightgray")+
  geom_sf_label(aes(label = sovereignt), label.padding = unit(1, "mm"))+
  geom_point(data = water_data, aes(x = lon_deg, y = lat_deg, color=water_tech),size=2, alpha=.5)+
  scale_color_manual(values = pal)+
  labs(title = 'Water access points in Central America - {current_frame}', 
       caption  = "Data Source: Tidy Tuesday & Water Point Data Exchange | Plot by Rafael Rocha")+
  theme_void()+
  theme(
      legend.title = element_blank(),
      plot.title = element_text(family = "Lato", size = 22, face = "bold"),
      legend.position = "bottom",
      legend.text = element_text(size = 16),
      plot.margin=unit(c(0,1,1,1),"cm")) +
  transition_manual(install_year, cumulative = TRUE)

# Refined animation specs

animate(p,duration=20 , 
        start_pause=1,
        end_pause = 30,
        detail=3,
        bg='LightSlateGray',
        type='cairo',
        renderer = gifski_renderer(),
        width=600,
        height=450
)

# Save animation

anim_save(filename = paste0("Water_system",Sys.Date(),".gif"),animation = last_animation())
