# Details ----
#' 9_news_words_igraph.R
#' Paper: [Est. extent of range contraction and extinction timeline]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 2025-12-1
#' Content: word associations!
#' + igraph
#' -----------

source('helpers/help_news.R')
trove_main <- source('data/xls/processed/trove_main.xlsx')

library(widyr)
library(igraph)
library(ggraph)

# word occurances----

tidy_title <- trove_main %>%
  unnest_tokens(word, Art_Title) %>%   
  anti_join(stop_words) 

title_word_pairs <- tidy_title %>% 
  pairwise_count(word, TVN, sort = TRUE, upper = FALSE)

set.seed(1234)
title_word_pairs %>%
  filter(n >= 5) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, 
                 point.padding = unit(0.2, "lines")) +
  theme_void()

#ooh! or the whole text ready?----
my_stop <- bind_rows(tibble(word = c("18", "18ft", 
                                     "6", "6in", 
                                     "16ft", "16",
                                     "5ft", "5", 
                                     "4", "4ft", "2"), 
                            lexicon = c("custom")), 
                     stop_words)

tidy_news <- trove_main %>%
  unnest_tokens(word, `Full text`) %>%   
  anti_join(my_stop)

text_word_pairs <- tidy_news %>% 
  filter(Main == 1) %>% 
  pairwise_count(word, TVN, sort = TRUE, upper = FALSE)

set.seed(1234)
text_word_pairs %>%
  filter(n >= 18) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, 
                 point.padding = unit(0.2, "lines")) +
  theme_void()

