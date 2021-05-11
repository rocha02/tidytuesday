library(ggplot2)
library(tidyverse)
library(skimr)
library(ggridges)
library(zipcodeR)


# Get the Data

tuesdata <- tidytuesdayR::tt_load(2021, week = 20)

broadband <- tuesdata$broadband
broadband_zip <- tuesdata$broadband_zip

# Check the data

glimpse(broadband)
glimpse(broadband_zip)
skim(broadband)
skim(broadband_zip)

# data manipulation

broadband <- broadband %>% 
  janitor::clean_names()
  
broadband_data <- broadband %>% 
  filter(broadband_usage !="-") %>% 
  mutate(broadband_usage = as.numeric(broadband_usage))

# plot

broadband_data %>% 
  ggplot(aes(x= reorder(st,broadband_usage), y = broadband_usage))+
  geom_boxplot()+
  theme_bw()+
  coord_flip()

broadband_data %>% 
  ggplot(aes(x= st, y = reorder(st,broadband_usage)))+   #ridge plot
  geom_density_ridges()+
  theme_bw()
