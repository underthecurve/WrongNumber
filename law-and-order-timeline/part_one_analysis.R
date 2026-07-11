library(tidyverse)
library(lubridate)
library(googlesheets4)
library(skimr)

## This is the sheet with all the data
sheet = "https://docs.google.com/spreadsheets/d/1Kx3jrh44nrAW4jhUkGhZAOQrtEzCqaCL_u3YwtJEWDI/edit"

episode.scene.cards <- read_sheet(sheet,
                                  sheet = 'scene_cards_data')

####### EXTRACT FIRST AND FINAL SCENE CARDS ####### 
first.last.card <- episode.scene.cards %>%
  mutate(final_card = coalesce(card_13, card_12, card_11, card_10, card_9, 
                               card_8, card_7, card_6, card_5, card_4, 
                               card_3, card_2, card_1)) %>%
  select(episode_number, title, original_airdate, url, first_card = card_1, final_card)

date_regex <- "(Monday|Tuesday|Wednesday|Thursday|Friday|Saturday|Sunday)(?:,\\s*|\\s+)(Jan(?:uary)?|Feb(?:ruary)?|Mar(?:ch)?|Apr(?:il)?|May|Jun(?:e)?|Jul(?:y)?|Aug(?:ust)?|Sep(?:tember)?|Oct(?:ober)?|Nov(?:ember)?|Dec(?:ember)?)\\s+\\d{1,2}(?:st|nd|rd|th)?(?:,\\s*\\d{4})?"

first.last.card.dates <- first.last.card %>%
  rowwise() %>%
  mutate(
    first_card_date = paste0(str_extract(first_card, regex(date_regex, ignore_case = TRUE)), " ", year(original_airdate)),
    final_card_date = paste0(str_extract(final_card, regex(date_regex, ignore_case = TRUE)), " ", year(original_airdate)
  )) %>%
  ungroup() %>%
  select(episode_number, title, original_airdate, url, first_card, final_card, first_card_date, final_card_date)

# first.last.card.dates %>% filter(is.na(first_card_date)) %>% View()
# first.last.card.dates %>% filter(is.na(final_card_date)) %>% View()
glimpse(first.last.card.dates)

first.last.card.dates <- first.last.card.dates %>% mutate(first_card_date = mdy(first_card_date),
                                                          final_card_date = mdy(final_card_date),
                                                          first_final_difference = final_card_date-first_card_date) %>%
  mutate(final_card_date_new = case_when(first_final_difference <0 ~ mdy(paste0(month(final_card_date),
                                                                                '-',
                                                                                day(final_card_date),
                                                                                '-',
                                                                                year(final_card_date)+1)),
                                         TRUE ~ final_card_date),
         first_final_difference_new = final_card_date_new-first_card_date)  %>%
  filter(first_final_difference_new != 0)

first.last.card.dates %>% select(episode_number, title, url,
                                 first_card_date, final_card_date = final_card_date_new) %>%
  write_sheet(sheet, sheet = 'first_final_scene_cards')

####### AVERAGE NUMBER OF DAYS ####### 
skim(first.last.card.dates$first_final_difference_new)
mean(first.last.card.dates$first_final_difference_new, na.rm = T)

mean_val <- mean(first.last.card.dates$first_final_difference_new, na.rm = TRUE)

####### CHART ####### 
ggplot(first.last.card.dates, aes(x = first_final_difference_new)) +
  geom_histogram(aes(y = after_stat(count) / sum(after_stat(count)) * 100),
                 fill = '#64A2E5',
                 color = 'white', alpha = 0.8, linewidth = 0.3) +
  scale_x_continuous(breaks = c(5, 50, 100, 150, 200, 250, 300),
                     labels = scales::label_number(suffix = " days")) +
  scale_y_continuous(labels = scales::label_number(suffix = "% of episodes"),
                     expand = c(0, 0),
                     breaks = c(5, 10, 15, 20)) +
  geom_vline(xintercept = mean_val, color = "red", 
             linetype = "solid", size = 1,
             alpha = 0.8) +
  annotate("text", 
           x = mean_val, 
           y = Inf,  
           label = paste("Average:", paste0(round(mean_val), " days")), 
           color = "red", 
           vjust = 2,  
           hjust = -.05) +
  labs(x = "", y = "", title = "Days elapsed over the course of a Law & Order episode",
       subtitle = "Based on an analysis of 415 episodes\n") +
  theme_minimal() + 
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold"),
    axis.line.x = element_line(color = '#363636'),
    axis.ticks.x = element_line(color = "#363636"), 
    axis.ticks.length.x = unit(0.2, "cm"),
    axis.text.y = element_text(
      vjust = -0.5,           # Shifts text upward to sit on the line
      margin = margin(r = -70) # Pulls text inward toward the plot area
    )
  )