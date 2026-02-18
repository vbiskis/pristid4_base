# Details ----
#' 9_news_words_selected.R
#' Paper: [Est. extent of range contraction and extinction timeline]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 2025-12-1
#' Content: common word/time *manually selected list
#' + This fig was first generated for reviewers and has been since replaced
#' + Manual plots of word usage over time by category
#' -----------

source('helpers/help_stat.R')
library(tidytext)
library(tm)

trove_all <- readxl::read_xlsx("data/xls/TroveDat_ALLArts.xlsx") 

### Define words you want to track----
bad_words <- c("monster", "struggle", "destroy", "savage", "beast", "peculiar",
               "unusual", "attack", "giant", "weapon", "enemy", "fight")

good_words <- c("exciting", "excite", "excitement", "won", "win", "trophy", "keen",
                "party", "enthusiastic", "sport", "good", "great",
                "prize", "eager", "nice", "record")

fishing_words <- c("net", "line", "boat", "harpoon", "despatch", "dinghy", "tow", "hook",
                   "shot", "trap", "mesh", "prawn", "trawl", "haul", "trawling", 
                   "bait", 'caught', 'landed', "netted", "capture")

generic_words <- c('fish', 'sawfish', 'sword', 'saw', 'sawfish', 'teeth', 'swordfish', 'river',
                   'fishermen', 'surf', 'shark', 'water', "point")

size_words <- c('feet', 'length', 'big', 'huge', 'measured', 'weigh', 'weighed', 
                "record", "long", "foot", "cwt", "pound", "pounds", "ton")


articles_per_year <- trove_all %>%
  count(Year_Art, name = "total_articles")

fish_year <- trove_all %>%
  dplyr::mutate(text = paste(`Full text`, 
                             Art_Title)) %>%
  unnest_tokens(word, text) %>%
  filter(word %in% fishing_words) %>%  # Only these words
  count(Year_Art, word, sort = TRUE) #add in TL calc here!

percent <- fish_year %>%
  left_join(articles_per_year, by = "Year_Art") %>%
  mutate(percentage = (n / total_articles) * 100) %>% 
  filter(Year_Art <= 1955,
         Year_Art >= 1882)

# Plot top words over time----
ggplot(percent, aes(x = Year_Art, y = percentage/100, fill = word)) +
  geom_area(position = "stack") +  
  labs(title = "", 
       color = "Emotive",
       x = "Year", 
       y = "Proportion of Articles") +
  theme_minimal() +
  scale_fill_brewer(type = "qual", palette = "Set3")  

ggsave(
  "figs/fig12/sawfishbad.tiff",
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 6,
  height = 4,
  units = c("in", "cm", "mm", "px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
)
