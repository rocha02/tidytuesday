library(tidytuesdayR)
library(dplyr)
library(ggplot2)
library(tidytext)

# Get the Data

tuesdata <- tidytuesdayR::tt_load(2021, week = 17)

netflix <- tuesdata$netflix

## Filtrar obras brasileiras

netflix_br <- netflix %>% 
  filter(country=="Brazil")



netflix_titles %>%
  unnest_tokens(word, description) %>%
  anti_join(get_stopwords()) %>%
  count(type, word, sort = TRUE) %>%
  group_by(type) %>%
  slice_max(n, n = 15) %>%
  ungroup() %>%
  mutate(word = reorder_within(word, n, type)) %>%
  ggplot(aes(n, word, fill = type)) +
  geom_col(show.legend = FALSE, alpha = 0.8) +
  scale_y_reordered() +
  facet_wrap(~type, scales = "free") +
  labs(
    x = "Word frequency", y = NULL,
    title = "Top words in Netflix descriptions by frequency",
    subtitle = "After removing stop words"
  )


## Wrangle data
# Add 'added year' column
n_year <- netflix %>%
  rowwise() %>%
  mutate(added_year = as.numeric(strsplit(date_added, split=",")[[1]][2]))

# Separate genres and pivot into one column, summarize based on added_year
n_genre <- n_year %>%
  mutate(listed_in = strsplit(as.character(listed_in), ", ")) %>%
  unnest(listed_in) %>%
  filter(type == "TV Show") %>%
  count(listed_in, added_year) %>%
  select(added_year, listed_in, n)

# find top 5 genres by count of shows
n_top5 <- n_year %>%
  mutate(listed_in = strsplit(as.character(listed_in), ", ")) %>%
  unnest(listed_in) %>%
  filter(type == "TV Show") %>%
  group_by(listed_in) %>%
  ungroup() %>%
  count(listed_in) %>%
  top_n(n=5) 

# join datasets
n_join <- n_genre %>%
  inner_join(n_top5, by = "listed_in")

# Set theme
font_family1 <- 'Century Gothic'
font_family2 <- 'Roadgeek 2005 Series 4B'
background <- "#000000"
text_colour1 <- "white"
text_colour2 <- "black"
axis_colour <- "white"
theme_style <- theme(text = element_text(family = font_family1),
                     rect = element_rect(fill = background),
                     plot.background = element_rect(fill = background, color = NA),
                     plot.title = element_text(size = 20, colour = text_colour1, family = font_family2),
                     plot.subtitle = element_text(size = 16, colour = text_colour1),
                     plot.caption = element_text(size = 12, colour = text_colour1, margin=margin(60,0,0,0)),
                     panel.background = element_rect(fill = background, color = NA),
                     panel.border = element_blank(),
                     panel.grid.major.y = element_blank(),
                     panel.grid.major.x = element_blank(),
                     panel.grid.minor.x = element_blank(),
                     plot.margin = unit(c(1, 1, 1, 1), "cm"),
                     axis.title = element_blank(),
                     axis.text.x = element_text(size = 14, colour= text_colour1),
                     axis.text.y = element_blank(),
                     axis.line = element_blank(),
                     axis.ticks = element_blank(),
                     legend.text = element_text(size = 10, colour= text_colour1),
                     legend.title = element_blank(),
                     legend.box = "horizontal",
                     legend.position="top",
                     legend.justification = "left")

theme_set(theme_classic() + theme_style)

# Set colour palette
cols <- c("#C39549", "#E60A14", "#10264D", "#5C2858", "#FEFFFE")
