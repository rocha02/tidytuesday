library(tidytuesdayR)
library(tidyverse)
library(dplyr)
library(ggplot2)
#library(sf)
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

water_la <- read_rds("2021/week 19/water_la.rds")

water <- water_la %>% 
  filter(country_name == c("El Salvador", "Mexico","Honduras", "Nicaragua")) 

water %>% 
  group_by(country_name, water_tech, install_year) %>%
  summarise(n = n())

countries <- 
  ne_countries(country = c("El Salvador","Honduras", "Nicaragua"), 
               scale = "large", returnclass = "sf") 

central_states <- 
  ne_states(iso_a2 = c("NI", "SV", "HN"), returnclass = "sf") 
  
# Plot

ghibli_pal <- ghibli::ghibli_palette("PonyoMedium", type = "discrete")[c(2:3, 5:7)]


water_ca <- water_la %>% 
  filter(country_name == c("El Salvador","Honduras", "Nicaragua")) %>% 
  mutate(water_tech = replace(water_tech, str_detect(water_tech, "Hand Pump"), "Hand Pump")) %>%
  mutate(water_tech = replace(water_tech, str_detect(water_tech, "NA"), "Not identified")) %>%
  mutate(water_tech = replace(water_tech, str_detect(water_tech, "Mechanized Pump"), "Mechanized Pump"))

p <- ggplot(central_paises) +
  geom_sf(fill = "gray")+
  geom_point(data = water, aes(x = lon_deg, y = lat_deg, color=water_tech),alpha=.5)+
  scale_fill_manual(values = ghibli_pal, na.value = "lightgray") +
  theme(axis.line = element_blank(),
        plot.subtitle = element_text(face="bold", size=15),
        plot.caption = element_text(face = "italic", size = 6, color = "grey"),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        legend.title = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        legend.position = c(0.16, 0.19), 
        #legend.title = element_text(size = 12),
        #legend.text = element_text(size = 11),
        text = element_text(family = "CMU Sans Serif"),
        plot.title = element_text(size = 24))+
  transition_manual(water_ca$install_year, cumulative = TRUE)



  labs(fill = "Primary system by state", 
       title = "Water Transportation Systems",
       subtitle = "How does water get from its source to the point of collection?",
       caption = "Data: Water Point Data Exchange") +
  theme(panel.background = element_rect(fill = "transparent"),
        legend.position = c(0.16, 0.19), 
        #legend.title = element_text(size = 12),
        #legend.text = element_text(size = 11),
        text = element_text(family = "CMU Sans Serif"),
        plot.title = element_text(size = 24))

p


+
  transition_time(install_year)

water_ca$install_year <- as.numeric(water_ca$install_year)




ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, colour = country)) +
  geom_point(alpha = 0.7) +
  scale_colour_manual(values = country_colors) +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  facet_wrap(~continent) +
  theme(legend.position = 'none') +
  labs(title = 'Year: {frame_time}', x = 'GDP per capita', y = 'life expectancy') +
  transition_time(year)


