library(ggplot2)
library(tidyverse)
library(skimr)
library(janitor)
library(urbnmapr)
library(viridis)

# Get the Data

tuesdata <- tidytuesdayR::tt_load(2021, week = 20)

broadband <- tuesdata$broadband
broadband_zip <- tuesdata$broadband_zip

# Check the data

glimpse(broadband)
glimpse(broadband_zip)
skim(broadband)
skim(broadband_zip)

# data cleaning

broadband <- broadband %>% 
  janitor::clean_names()
  
broadband_data <- broadband %>% 
  filter(broadband_usage !="-") %>% 
  mutate(broadband_usage = as.numeric(broadband_usage)) %>% 
  mutate(broadband_availability_per_fcc = as.numeric(broadband_availability_per_fcc))

broadband_zip <- broadband_zip %>% 
  janitor::clean_names()

# create counties data frame

counties <- get_urbn_map(map = "counties", sf = TRUE) %>% 
  mutate(county_fips = as.numeric(county_fips))

glimpse(counties)

# plot

g <- counties %>%
  left_join(broadband_data, by = "county_name") %>%
  ggplot() +
  geom_sf(mapping = aes(fill = broadband_availability_per_fcc), color = "#b2b2b2", size = 0.01) +
  coord_sf(datum = NA)+
  scale_fill_viridis(option="plasma", breaks = c(.09,.2,.4,.6,.8,1), 
                     labels = c("> 9%", "20%", "40%","60%","80%","< 80%"))+
  theme_minimal(base_size = 12 ) +
  theme(plot.subtitle = element_text(family = "Lato",size = 12, hjust = 0.5),
        plot.title = element_text(family = "Lato", size = 20, face = "bold",hjust = 0.5),
        legend.text = element_text(family = "Lato",size = 10),
        legend.title = element_text(family = "Lato", size = 12),
        plot.margin = margin(t = 1, r = 1, b = 1, l = 1, 
                             unit = "cm"),
        legend.margin = margin(t = 2, r = 2, b = 5, l = 2, 
                               unit = "mm")) +
  guides(fill = guide_legend(title = "% of population",
                             keywidth=unit(6, units = "mm"),
                             title.hjust = 1,
                             reverse = TRUE,
                             barwidth = 15,
                             barheight = 4)) +
  labs(fill ="", title = "High Speed Broadband Availability in US", 
       subtitle = str_wrap("% of people per county with access to fixed terrestrial 
                           broadband at speeds of 25 Mbps/3 Mbps - 2017"),
       caption = "Data Source: Tidy Tuesday & Microsoft | Plot by Rafael Rocha")

ggsave(filename = "broadband_14.05.21.png", plot = last_plot(), width = 10, height = 6)
