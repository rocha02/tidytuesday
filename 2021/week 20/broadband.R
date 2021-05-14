library(ggplot2)
library(tidyverse)
library(skimr)
library(zipcodeR)
library(janitor)
library(urbnmapr)
library(tidyr)


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

mean_broadband <- broadband %>% 
  group_by(st) %>% 
  summarise(avg_broadband_availability = mean(as.numeric(broadband_availability_per_fcc), na.rm=T))


# create counties data frame

counties <- get_urbn_map(map = "counties", sf = TRUE) %>% 
  mutate(county_fips = as.numeric(county_fips))

glimpse(counties)


# plot

library(viridis)

g <- counties %>%
  left_join(broadband_data, by = "county_name") %>%
  ggplot() +
  geom_sf(mapping = aes(fill = broadband_usage), color = "#b2b2b2", size = 0.01) +
  coord_sf(datum = NA)+
  scale_fill_viridis(option="plasma", breaks = c(.09,.2,.4,.6,.8,1), 
                     labels = c("> 9%", "20%", "40%","60%","80%","< 81%"))+
  theme_minimal(base_family = "Source Sans Pro")+
  theme(plot.margin = margin(t = 20,r =10,b = 20,l = 20),
        plot.title = ggtext::element_markdown(size = rel(1.8), face = "bold", 
                                              hjust = .5, margin = margin(b = 20)),
        plot.caption = ggtext::element_markdown(family = "Source Sans Pro",size = rel(1))
  )
    
    
  
    legend.position = c(0.985, 0.5), 
        



                     name="% of people per county with access", 
                     guide = guide_legend(keyheight = unit(7, units = "mm"), 
                                           keywidth=unit(12, units = "mm"), 
                                           label.position = "bottom", 
                                           title.position = 'top', nrow=1))+
  
theme <-  theme_minimal(base_family = "Source Sans Pro") + 
  theme(
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text =  element_blank(),
    panel.grid = element_blank(),
    plot.margin = margin(t = 20,r =10,b = 20,l = 20),
    plot.title = ggtext::element_markdown(size = rel(1.8), face = "bold", hjust = .5, margin = margin(b = 20)),
    plot.subtitle = ggtext::element_markdown(size = rel(1.2), lineheight = 1.2, family = "Bembo Std",face = "italic", hjust = .5),
    plot.caption = ggtext::element_markdown(family = "Source Sans Pro",size = rel(1))
  )


+ 
  scale_fill_gradientn(labels = scales::percent,
                       colours = wes_palette("Darjeeling1", 100, type = "continuous"),
                       na.value = "grey",
                       trans = "reverse") +
  theme_minimal(base_size = 10 ) +
  theme(plot.subtitle = element_text(size = 8, family = "oxy", hjust = 0.5),
        plot.title = element_text(size = 14, family = "buntu", face = "bold")) +
  guides(fill = guide_colorbar(title.position = "top",
                               title.hjust = .5,
                               reverse = TRUE,
                               barheight = unit(10, "lines"),
                               barwidth = unit(.5, "lines"))) +
  labs(fill ="", title = "Broadband Availability", 
       subtitle = str_wrap("% of people per county with access to fixed terrestrial broadband at speeds of 25 Mbps/3 Mbps as of the end of 2017"))
          


map <- broadband_sf %>%  
  ggplot() +
  +
  labs(
    title = "Percentage of people per county that use the internet at broadband speeds",
    subtitle = summary_text,
    fill = NULL
  ) + 
  scale_fill_gradientn(colours = pnw_palette("Bay",100), guide = "coloursteps",
                       breaks = c(.2,.4,.6,.8), 
                       labels = c("≤ 20%","","","≥ 80%")) +
  coord_sf(crs = "ESRI:102003") +
  my_theme +
  theme(legend.position = c(0.985, 0.5), 
        legend.text = element_text(face = "bold"))



plot_il(estimate) +
  scale_fill_viridis_b(option = "plasma", n.breaks = 8, labels = scales::comma) +
  labs(
    subtitle = "Median Household Income",
    fill = "$US"
  ) +
  plot_il(usage) +
  scale_fill_viridis_b(n.breaks = 8, labels = scales::percent_format(accuracy = 1)) +
  labs(
    subtitle = "High Speed Broadband Usage",
    fill = NULL
  ) +
  plot_annotation(
    title = "High Speed Broadband in Illinois",
    caption = "Data Sources: Microsoft and US Census ACS 2019 \n
                  Percent of people per county that use the internet at more than 25 Mbps/3 Mbps"
  ) &
  theme(
    plot.margin = margin(0.5, 0, 0, 0, "cm"),
    plot.title = element_text(size = 30)
  )
