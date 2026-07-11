library(janitor)
library(tidyverse)
library(lubridate)
library(googlesheets4)
library(forcats)
library(skimr)

## This is the sheet with all the data
sheet = "https://docs.google.com/spreadsheets/d/1Kx3jrh44nrAW4jhUkGhZAOQrtEzCqaCL_u3YwtJEWDI/edit"


###### For L&O, compare Tribble314 data with data from scene cards. When there is divergence, pick the longer one ###### 
episodes.combined.distinct.clean <- read_sheet(sheet, sheet = 'tribble314_data') # cleaned data from Tribble314 (https://www.thecomicboard.com/threads/law-order-timeline.14797/)

episodes.combined.distinct.clean <- episodes.combined.distinct.clean %>%
  mutate(diff = (difftime(end_date, start_date, units = "days")))

first.final.cards <- read_sheet(sheet, sheet = 'first_final_scene_cards') %>%
  mutate(diff = final_card_date-first_card_date)

comparison <- episodes.combined.distinct.clean %>% 
  filter(Series == 'L&O' & season == 1) %>%
  select(Series, Ep, season, episode, Title, url, diff, start_date, end_date) %>%
  merge(first.final.cards %>% select(url, first_card_date, final_card_date, diff),
        by = 'url',
        all.x = T,
        suffixes = c('_tribble314', '_scenecards')) %>%
  mutate(time_comp = case_when(diff_scenecards > diff_tribble314 ~ 'scene cards have longer time elapsed',
                               diff_scenecards < diff_tribble314 ~ 'tribble314 has longer time elapsed',
                               diff_scenecards == diff_tribble314 ~ 'same amount of time'))
comparison %>% filter(time_comp == 'scene cards have longer time elapsed')
comparison %>% filter(first_card_date < start_date)

comparison <- comparison %>% 
  mutate(
    end_date_final = case_when(
      time_comp == 'scene cards have longer time elapsed' ~ final_card_date, 
      TRUE ~ end_date
    )
  ) %>%
  mutate(
    start_date_final = case_when(
      Title == 'Happily Ever After' ~ first_card_date, 
      TRUE ~ start_date
    )
  ) %>%
  mutate(
    end_date_final = case_when(
      Title == 'A Death in the Family' ~ ymd('1990-11-20'),
      TRUE ~ as.Date(end_date_final) # Force to Date class
    ),
    # Ensure start_date is also a Date to avoid the clash
    diff_final = (as.Date(end_date_final) - as.Date(start_date))
  )

skim(comparison$diff_final)
mean(comparison$diff_final)

skim(comparison$diff_scenecards)
mean(comparison$diff_scenecards, na.rm = T)

comparison <- comparison %>% mutate(Title2 = paste0(Ep, " ", Title))

###### CHARTS ###### 
ggplot(comparison,
       aes(y = fct_reorder(Title2, episode, .desc = TRUE), 
           x = as.Date(start_date_final))) +
  geom_segment(aes(yend = Title2, xend = as.Date(end_date_final)), 
               linewidth = 5, # Changed 'size' to 'linewidth'
               alpha = 0.8, color = '#64A2E5') +
  labs(x = '', y = '', title = 'Timeline of Law & Order Season 1 cases',
       subtitle = 'Start and end dates of the events taking place in each episode') +
  scale_x_date(date_breaks = '3 months',
               date_labels = '%b %Y') +
  theme_minimal(base_family = "Helvetica") + # Sets font family for the whole plot
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "#555555", size = 11),
    axis.text = element_text(size = 10),
    axis.line.x = element_line(color = '#363636'),
    axis.ticks.x = element_line(color = "#363636"), 
    axis.ticks.length.x = unit(0.2, "cm"),
    axis.text.y = element_text(hjust = 0, margin = margin(r = -10))
  )

episodes.combined.distinct.clean <- episodes.combined.distinct.clean %>% mutate(Title2 = paste0(Ep, " ", Title))


ggplot(episodes.combined.distinct.clean %>% filter(Series == 'SVU' & season == 24 & episode != 1),
       aes(y = fct_reorder(Title2, episode, .desc = TRUE), 
           x = as.Date(start_date))) +
  geom_segment(aes(yend = Title2, xend = ymd(end_date)), 
               linewidth = 5, 
               alpha = 0.8, color = '#64A2E5') +
  labs(x = '', y = '', title = 'Timeline of Law & Order: SVU Season 24 cases',
       subtitle = 'Start and end dates of the events taking place in each episode') + 
  scale_x_date(date_breaks = '1 month',
               date_labels = '%b %Y',
               expand = expansion(mult = c(0.02, 0.02))) + 
  scale_y_discrete(expand = expansion(add = c(0.6, 0.6))) + 
  theme_minimal(base_family = "Helvetica") + 
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "#555555", size = 11),
    axis.text = element_text(size = 10),
    axis.line.x = element_line(color = '#363636'),
    axis.ticks.x = element_line(color = "#363636"), 
    axis.ticks.length.x = unit(0.2, "cm"),
    axis.text.y = element_text(hjust = 1), 
    plot.margin = margin(t = 15, r = 15, b = 15, l = 15) 
  )

episodes.combined.distinct.clean %>% filter(Series == 'SVU' & season == 24 & episode != 1) %>%
  select(diff) %>% skim()

episodes.combined.distinct.clean %>% filter(Series == 'SVU' & season == 24 & episode != 1) %>%
  select(diff) %>% summary()

ggplot(episodes.combined.distinct.clean %>% filter(Series == 'L&O' & season == 22 & episode != 1),
       aes(y = fct_reorder(Title2, episode, .desc = TRUE), 
           x = as.Date(start_date))) + 
  geom_segment(aes(yend = Title2, xend = ymd(end_date)), 
               linewidth = 5, 
               alpha = 0.8, color = '#64A2E5') +
  labs(x = '', y = '', 
       title = 'Timeline of Law & Order Season 22 cases',
       subtitle = 'Start and end dates of the events taking place in each episode') + # Caption removed here
  scale_x_date(date_breaks = '3 months',
               date_labels = '%b %Y',
               expand = expansion(mult = c(0.02, 0.02))) + 
  scale_y_discrete(expand = expansion(mult = c(0.06, 0.02))) + 
  theme_minimal(base_family = "Helvetica") + 
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "#555555", size = 11),
    axis.text = element_text(size = 10),
    axis.line.x = element_line(color = '#363636'),
    axis.ticks.x = element_line(color = "#363636"), 
    axis.ticks.length.x = unit(0.2, "cm"),
    axis.text.y = element_text(hjust = 0, margin = margin(r = 10)),
    plot.margin = margin(t = 10, r = 15, b = 15, l = 10)
  )

episodes.combined.distinct.clean %>% filter(Series == 'L&O' & season == 22 & episode != 1) %>%
  select(diff) %>% skim()

episodes.combined.distinct.clean %>% filter(Series == 'L&O' & season == 22 & episode != 1) %>%
  select(diff) %>% summary()
