# Details ----
#' 9_news_words.R
#' Paper: [Est. extent of range contraction and extinction timeline]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 2025-12-1
#' Content: word analysis 
#' + word clouds
#' -----------

source('helpers/help_news.R')
trove_main <- readxl::read_xlsx("data/xls/processed/trove_m_arts.xlsx") #all words

## quick filter----
tidy_news <- trove_main %>%
  unnest_tokens(word, `Full text`) %>%   
  anti_join(stop_words)  # Remove common words like "the" etc.

tidy_title <- trove_main %>%
  unnest_tokens(word, Art_Title) %>%   
  anti_join(stop_words)

# wordclouds----
library(reshape2)

##title----
tidy_title %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

##comparison----
#tiff("figs/fig12/other_vs/cloudtit.tiff", width = 2000, height = 2000, res = 300)
png("figs/fig12/other_vs/cloudtit.png", width = 2000, height = 2000, res = 300)

tidy_title %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 100, scale = c(6, 1))

dev.off()

## main text----
tidy_news %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

#tiff("figs/fig12/other_vs/cloudtxt.tiff", width = 3000, height = 3000, res = 300)
png("figs/fig12/other_vs/cloudtxt.png", width = 3000, height = 3000, res = 300)

tidy_news %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 100, scale = c(6, 1))

dev.off()
