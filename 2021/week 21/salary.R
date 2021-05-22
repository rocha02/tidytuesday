library(ggplot2)
library(tidyverse)
library(stringr)
library(forcats)
library(ggtext) 
library(patchwork)
library(hrbrthemes)

# Get the data

tuesdata <- tidytuesdayR::tt_load(2021, week = 21)

survey <- tuesdata$survey

# Check the data

glimpse(survey)

dist <- survey %>%
  filter(currency=="USD") %>% 
  count(industry, job_title)

survey <- survey %>% 
  mutate(highest_level_of_education_completed = recode(highest_level_of_education_completed,
                                              "Master's degree" = "MA degree",
                                              "NA" = "Other"))

# Filtering by industries: nonprofit & academia

nonprofit <- survey %>%  
  filter(currency == "USD" & industry == "Nonprofits") %>%  # filter nonprofit in industry
  mutate(nonp_acad = case_when(industry == "Nonprofits" ~ "Nonprofit")) # create a new column

academia <- survey %>%
  filter(currency == "USD" & industry == "Education (Higher Education)") %>% # filter Higher Education in industry
  mutate(nonp_acad = case_when(industry == "Education (Higher Education)" ~ "Academia")) # create a new column

academia1<- survey %>%  
  filter(str_detect(industry, pattern = ".ademi."))   # filter other academic areas in industry

academia1$nonp_acad <- "Academia" 
         
nonprofit_acad <- rbind(nonprofit,academia, academia1) # bind the nonprofit and academic data

glimpse(nonprofit_acad)
unique(nonprofit_acad$nonp_acad)

# Reducing the categorical variables (race and years of experience)

nonprofit_acad <- nonprofit_acad %>% # reduce the race to 5 categories
  mutate(race = ifelse(race == "Another option not listed here or prefer not to answer", "Other", race),
         race = fct_lump(race, 4))

nonprofit_acad <- nonprofit_acad %>% # reduce the race to 5 categories
  mutate(years_of_experience_in_field = case_when((years_of_experience_in_field 
                                                  %in% c("1 year or less", "2 - 4 years") ~ "5 years of less"),
                                                  (years_of_experience_in_field %in% c("5-7 years","8 - 10 years") ~ "5 - 10 years"),
         TRUE ~ (years_of_experience_in_field)))

# Adapting the theme

my_theme <- theme_ft_rc()+
  theme(legend.position = "none",
        legend.background = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.title = element_markdown())

my_colors <- c("#2ca1db", "#de425b")

# Create separate plots

# gender

p1 <- nonprofit_acad %>%
  filter(!is.na(gender)) %>% 
  filter(annual_salary < 150000 & gender!= "Other or prefer not to answer") %>% 
  ggplot(aes(x= annual_salary, y= fct_reorder(gender, annual_salary), fill=nonp_acad)) + 
  geom_violin(draw_quantiles = 0.5, trim = TRUE, position="dodge", 
              alpha=0.7)+
  scale_x_continuous(breaks=c(0, 50000, 100000, 150000), 
                        labels=c("0", "50K", "100K", "150K"), limits=c(-1000,150000))+
  labs(title="Gender",
       x = '', 
       y = '')+
  scale_fill_manual(values = my_colors) + 
  my_theme + 
  coord_flip()

# Race

p2 <- nonprofit_acad %>%
  filter(!is.na(race)) %>%
  filter(annual_salary < 150000) %>% 
  ggplot(aes(x=annual_salary, y= fct_reorder(race, annual_salary), fill=nonp_acad)) + 
  geom_violin(draw_quantiles = 0.5, trim = TRUE, position="dodge", 
              alpha=0.7)+
  scale_x_continuous(breaks=c(0, 50000, 100000, 150000), 
                     labels=c("0", "50K", "100K", "150K"), limits=c(-1000,150000))+
  labs(title = "Race",
       x = '', 
       y = '')+
  scale_fill_manual(values = my_colors) + 
  scale_y_discrete(labels = function(y) str_wrap(y, width = 14))+
  my_theme + 
  coord_flip()
  
# level of education

p3 <- nonprofit_acad %>%
  filter(!is.na(highest_level_of_education_completed)) %>% 
  filter(annual_salary < 150000 & highest_level_of_education_completed != "Some college") %>% 
  ggplot(aes(x=annual_salary, y= fct_reorder(highest_level_of_education_completed, annual_salary), fill=nonp_acad)) + 
  geom_violin(draw_quantiles = 0.5, trim = TRUE, position="dodge", 
              alpha=0.7)+
  scale_x_continuous(breaks=c(0, 50000, 100000, 150000), 
                     labels=c("0", "50K", "100K", "150K"), limits=c(-1000,150000))+
  labs(title ="Level of Education",
       x = '', 
       y = '')+
  scale_fill_manual(values = my_colors) + 
  scale_y_discrete(labels = function(y) str_wrap(y, width = 20))+
  my_theme + 
  coord_flip()

# experience

p4 <- nonprofit_acad %>%
  filter(!is.na(years_of_experience_in_field)) %>% 
  filter(annual_salary < 200000) %>% 
  ggplot(aes(x=annual_salary, y= fct_reorder(years_of_experience_in_field, annual_salary), fill=nonp_acad)) + 
  geom_violin(draw_quantiles = 0.5, trim = TRUE, position="dodge", 
              alpha=0.7)+
  scale_x_continuous(breaks=c(0, 50000, 100000, 150000), 
                     labels=c("0", "50K", "100K", "150K"), limits=c(-1000,150000))+
  labs(title ="Years of Experience in Field",
       x = '', 
       y = '')+
  scale_fill_manual(values = my_colors) + 
  scale_y_discrete(labels = function(y) str_wrap(y, width = 10))+
  my_theme + 
  coord_flip()

# Use patchwork to merge the plots

plot <- p1+p2+p3+p4 +
  plot_layout(widths = 6)+
  plot_annotation(
  title = "A Comparison of Annual Salary in <span style='color:#2ca1db'>Academia</span> and 
    <span style='color:#de425b'>Nonprofits</span> by:",
    subtitle = "Annual Salary in US Dollars ($)",
    caption = 'Tidy Tuesday Week 21\nData: Ask a Manager Survey | Plot by Rafael Rocha') &
    my_theme+
  theme(plot.title = element_markdown(size = 30),
        plot.subtitle = element_text(size = 18),
        plot.caption = element_text(size = 12))

ggsave(("./2021/week 21/salary.png"), dpi = 320, height = 12, width = 16)