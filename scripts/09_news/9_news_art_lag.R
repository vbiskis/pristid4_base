# Details ----
#' 9_news_art_lag.R
#' Paper: [Est. extent of range contraction and extinction timeline]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 2025-12-1
#' Content: lag time from event occurrence
#' + supp plot - lag time after reports
#' -----------

source('helpers/help_news.R')
trove_main <- readxl::read_xlsx("data/xls/processed/trove_main.xlsx") 

library(ggdist)
library(gghalves)
library(ggbeeswarm)

# a silly time graph----
time_diffs <- trove_main %>%
  filter(!is.na(date_ev)) %>% 
  group_by(SFN) %>%
  mutate(
    first_date = min(date_a, na.rm = TRUE),
    days_after_first = as.numeric(date_a - first_date),
    days_after_event = as.numeric(date_a - date_ev),
  ) %>%
  filter(days_after_first > 0) %>%  # Only keep the duplicates, not the first one
  ungroup() 

#ooh is that sig??
time_diffs_no <- time_diffs %>% 
  filter(days_after_event <= 365)

# Create the plot
ggplot(data = time_diffs_no,
       aes(x = Period,
           y = days_after_event,
           colour = Period,
           fill = Period)) +
  #facet_grid(~NP_BasinCap) +
  ggdist::stat_halfeye(adjust = .4,
                       width = .87,
                       colour = NA) +
  gghalves::geom_half_point(side = "l",       # choose right or left side
                            range_scale = .3, # spread of points
                            alpha = .6,
                            size = 2.2) +
  geom_boxplot(
    aes(colour = Period,
        colour = after_scale(colorspace::darken(colour, .7))),
    width = .12,
    fill = NA,
    size = 1.1,
    outlier.shape = NA
  ) +
  coord_flip() +
  scale_y_log10() +
  labs(x = "Period",
       y = "Days after sawfish interaction") +
  pilot::scale_color_pilot() +
  pilot::scale_fill_pilot() +
  pilot::theme_pilot(grid = "",
                     axes = "b") +
  theme(legend.position = "none")


# stats----
summary(lm(log(days_after_event) ~ Year_Art, time_diffs_no)) #no 
summary(lm(log(days_after_event) ~ TL_final, time_diffs_no)) #no

kruskal.test(days_after_event ~ Period, time_diffs_no) #yes
time_diffs_no %>% 
  group_by(Period) %>% 
  summarise(
    rep_lag = mean(days_after_event, na.rm = TRUE),
    rep_sd = sd(days_after_event, na.rm = TRUE)
  )