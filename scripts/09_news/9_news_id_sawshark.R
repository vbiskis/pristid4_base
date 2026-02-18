# Details ----
#' 9_news_id_sawshark.R
#' Paper: [Est. extent of range contraction and extinction timeline]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 2025-12-1
#' Content: word analysis: sawfish ID
#' + supps: sawshark/swordfish
#' -----------

source('helpers/help_news.R')
filthist <- readxl::read_xlsx("data/xls/processed/trove_main.xlsx") 

#id graph----
id_year <- trove_main %>%
  dplyr::mutate(text = tolower(paste(`Full text`, Art_Title))) %>%
  mutate(
    has_swordfish = str_detect(text, "sword.fish|swordfish"),
    has_sawfish = str_detect(text, "saw.fish|sawfish"),
    has_sawshark = str_detect(text, "saw.shark|sawshark")
  ) %>%
  pivot_longer(cols = starts_with("has_"), 
               names_to = "word_type", 
               values_to = "present") %>%
  filter(present == TRUE) %>%
  mutate(word = str_remove(word_type, "has_")) %>%
  count(Year_Art, word, sort = TRUE)

id_percent <- id_year %>%
  left_join(articles_per_year, by = "Year_Art") %>%
  mutate(percentage = (n / total_articles) * 100) %>% 
  filter(Year_Art <= 1955,
         Year_Art >= 1875)

# Plot top words over time
ggplot(id_percent, aes(x = Year_Art, y = percentage, fill = word)) +
  geom_area(position = "fill", alpha = 1) +  
  labs(title = "", 
       fill = "Common Name Used",
       x = "Year", 
       y = "Proportion of Articles") +
  theme_minimal() +
  theme(
    legend.position = "bottom"
  ) +
  scale_fill_brewer()

ggsave(
  "figs/s11/swordfishj.tiff",
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