# Details ----
#' 9_words_freq_heat.R
#' Paper: [Est. extent of range contraction and extinction timeline]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 2025-12-1
#' Content: timeline of common terms
#' + Figure 12
#' *Final fig for thesis
#' -----------

source('helpers/help_news.R')
art_sent <- readRDS('data/rds/art_sent.rds')
arts_sfn <- readRDS('data/rds/arts_sfn.rds')

library(cowplot)

# common words----
art_sent <- art_sent %>%
  mutate(decade = floor(Year_Art / 10) * 10) %>%
  filter(decade >1870, decade < 1970) 

common_words <- art_sent %>%
  group_by(decade) %>% 
  filter(n() > 2) %>%
  ungroup() %>% 
  count(decade, word, sentiment) %>%
  ungroup()

top_word <- common_words %>%
  filter(sentiment %in% c("negative", "positive"), 
         !word %in% c('shape', 'professional', # 'protector',
                      'visitor', 'obvious', 'captain')) %>%
  group_by(decade, sentiment) %>% 
  slice_max(n, n = 12) %>%  #gets top 10 words for each decade, 
#so now we can pull things that may not be common earlier/later
  ungroup() %>%
  count(word, sentiment, name = "decades_in_top10") %>%
  group_by(sentiment) %>%
  slice_max(decades_in_top10, n = 10, with_ties = FALSE) %>%
  ungroup()

# Track these words over time
total_articles <- art_sent %>%
  distinct(SFN, decade) %>%
  count(decade, name = "total_articles") #how many total articles in a decade

word_trends <- art_sent %>%
  filter(word %in% top_word$word) %>%
  dplyr::select(-sentiment) %>% #already has those cols
  left_join(top_word %>% 
              dplyr::select(word, sentiment), by = "word") %>%
  distinct(SFN, word, sentiment, decade) %>% #sawfish events that use the word
  count(word, sentiment, decade) %>%
  left_join(total_articles, by = "decade") %>%
  mutate(pct = n / total_articles) %>%
  dplyr::select(-total_articles)

# final plot!----
a <- ggplot(data = word_trends %>% 
         mutate(sentiment = str_to_title(sentiment))) +
    geom_tile(aes(x = decade, y = word, fill = pct)) +
    facet_grid(sentiment ~ ., 
           scales = "free_y", 
           space = "free_y",
           switch = "y") +
    scale_fill_met_c(name = 'Hokusai1', direction = -1, colors = 5) +
    scale_y_discrete(limits = rev) +  
    labs(x = "Decade",
     y = NULL,
     fill = "% of Articles") +
    pilot::theme_pilot(grid = "", facet_title_size = 14,
                   axis_title_size = 12, legend_title_size = 12,
                   axis_title_family = "optima", legend_title_family = "optima",
                   axis_text_family = "optima", legend_text_family = "optima") +
    theme(strip.text.y.left = element_text(angle = 90, 
                                           margin = margin(l = 5, r = 6), 
                                           family = "optima"),
      strip.placement = "outside",
      strip.background = element_rect(fill = NA, color = "black"),
      axis.text.y = element_text(margin = margin(l = 10)),
      legend.position = "right", legend.margin = margin(0, 5, 0, 0),
      legend.box.spacing = unit(1, "pt"),
      plot.margin = margin(5, 0, 0, 25))

b <- ggplot(data = arts_sfn,
       aes(x = year)) +
  geom_density() + 
  labs(x = "Year", y = "Number of\nEvents Reported") +
  scale_x_continuous(limits = c(1880, 1960)) +
  pilot::theme_pilot(grid = "h", axes = "b", 
                     axis_title_size = 12, 
                     axis_title_family = "optima", axis_text_family = "optima") +
  theme(axis.text.y = element_text(margin = margin(l = 10, r = 10)),
        plot.margin = margin(5, 0, 5, 20))

plot_grid(a, 
          b, ncol = 1, align = "v", axis = "lr", 
          rel_heights = c(2.5, 1), labels = c('a)', 'b)'), label_fontface = "plain",
          label_fontfamily = "optima")


ggsave(
  "f12_word_heat.png",
  plot = last_plot(),
  device = NULL,
  path = "figs/fig12/",
  scale = 1,
  width = 7,
  height = 7,
  units = c("in", "cm", "mm", "px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
)

## alt figure wordfreq----
library(patchwork)

p_neg <- ggplot(data = word_trends %>% 
                  filter(sentiment == "negative"), 
                aes(x = decade, y = n, fill = word)) +
  geom_area(position = "fill") +  
  labs(x = "Decade",
       y = "Proportion of Events Using Word") +
  scale_fill_met_d(name = 'Manet') +
  pilot::theme_pilot(grid = "", axes = "b") +
  theme(legend.position = "right")

p_pos <- ggplot(data = word_trends %>% 
                  filter(sentiment == "positive"), 
                aes(x = decade, y = n, fill = word)) +
  geom_area(position = "fill") +  
  labs(x = "Decade",
       y = "Proportion of Events Using Word") +
  scale_fill_met_d(name = 'Redon') +
  pilot::theme_pilot(grid = "", axes = "b") +
  theme(legend.position = "right")

p_neg + p_pos & theme(plot.margin = margin(5, 5, 5, 5))

ggsave(
  "wordfreq_stack.png",
  plot = last_plot(),
  device = NULL,
  path = "figs/fig12/other_vs/",
  scale = 1,
  width = 9,
  height = 5,
  units = c("in", "cm", "mm", "px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
)
