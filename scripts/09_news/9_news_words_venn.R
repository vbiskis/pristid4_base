# Details ----
#' 9_news_words_common.R
#' Paper: [Est. extent of range contraction and extinction timeline]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 2025-12-1
#' Content: venn diagram for supps
#' 
#' -----------

source('helpers/help_news.R')
art_sent <- readRDS('data/rds/art_sent.rds')

# common words----
out_of_bounds <- art_sent %>%
  filter(sentiment %in% c("negative", "positive")) %>% 
  distinct(word, sentiment) %>%
  count(word) %>%
  filter(n > 1) %>%  # appears in both sentiments
  pull(word)

art_sent <- art_sent %>%
  mutate(decade = floor(Year_Art / 10) * 10) %>%
  filter(decade >1870, decade < 1970) 

common_words <- art_sent %>%
  group_by(decade) %>% 
  filter(n() > 2) %>%
  ungroup() %>% 
  count(decade, word, sentiment) %>%
  ungroup()

## side plot sorry----

library(ggvenn)

# Create a list of words by sentiment
word_lists <- common_words %>%
  filter(sentiment %in% c("joy", "anger", "fear", "anticipation"),
         word!= 'letter', word != 'daily') %>%
  group_by(sentiment) %>%
  slice_max(n, n = 20) %>%
  ungroup() %>%
  split(.$sentiment) %>%
  map(~pull(.x, word))

# Check the names
names(word_lists)

word_lists <- word_lists[c("anger", "fear", "anticipation", "joy")]

v <- ggvenn(word_lists, 
            show_elements = TRUE,
            label_sep = "\n",
            stroke_size = 0.5,
            set_name_size = 6,
            fill_color = met.brewer("Manet", 4),
            fill_alpha = 0.25,
            text_size = 4,
            show_percentage = FALSE,
            max_elements = 20) +
  coord_cartesian(ylim = c(-0.25, 1), clip = "off")

ggsave("figs/s11/venn_diagram.png", 
       plot = v, width = 10, 
       height = 6, dpi = 300)
