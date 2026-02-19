# Details ----
#' 9_news_words_pca.R
#' Paper: [Est. extent of range contraction and extinction timeline]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 2025-12-1
#' Content: word analysis - PCA
#' + Figure ...
#' Not sure needed yet, but saving
#' -----------

source('helpers/help_news.R')
art_sent <- readRDS('data/rds/art_sent.rds') # all sentiment words per article (massive, multiple rows)
arts_wsnt <- readRDS('data/rds/arts_wsnt.rds') # articles just with all words in one col

## PCA----
library(stats)
library(ggfortify)
library(ggrepel)

# Step 1: Create word matrix from art_sent
word_matrix <- art_sent %>%
  filter(Year_Art < 1965,
         sentiment %in% c('negative', 'positive')) %>%
  group_by(word) %>%
  filter(n() > 10) %>%  # word appears in at least 10 articles
  ungroup() %>%
  count(TVN, word) %>%
  pivot_wider(names_from = word, 
              values_from = n, 
              values_fill = 0)

# Step 2: Run PCA
pca_data <- word_matrix %>%
  dplyr::select(-TVN) %>%
  prcomp(scale. = TRUE)

# Step 3: Join back to arts_wsnt for metadata
pca_scores <- as.data.frame(pca_data$x) %>%
  bind_cols(word_matrix %>% 
              dplyr::select(TVN)) %>%
  left_join(arts_wsnt %>% 
              filter(Year_Art < 1965) %>%
              dplyr::select(TVN, SFN, Period, net_sent, 
                            Year_Art, NP_Name_Fin, NP_Region, TL_final), 
            by = "TVN") %>% 
  mutate(Period = factor(Period, levels = c("Pre-1900", "1901-1915",
                                            "1916-1930", "1931-1945", 
                                            "1946-1960")))

# Step 4: Get top word loadings for arrows
loadings <- as.data.frame(pca_data$rotation[, 1:2])
loadings$word <- rownames(loadings)

top_loadings <- loadings %>%
  slice_max(PC1, n = 2) %>%
  bind_rows(
    loadings %>% slice_min(PC1, n = 2)
  ) %>%
  bind_rows(
    loadings %>% slice_max(PC2, n = 2)
  ) %>%
  bind_rows(
    loadings %>% slice_min(PC2, n = 2)
  ) %>%
  distinct(word, .keep_all = TRUE)

pca_scores_plot <- pca_scores %>%
  filter(abs(PC1) < 2, abs(PC2) < 2) 

# Step 5: Plot
ggplot(pca_scores_plot, aes(x = PC1, y = PC2)) +
  geom_point(aes(color = Period), alpha = 0.6) +
  stat_density_2d(aes(fill = Period), 
                  geom = "polygon",
                  contour_var = "ndensity",
                  breaks = c(0.5, 0.75, 0.95),  # top 50% density
                  alpha = 0.3) + 
  # Line contours 
  stat_density_2d(aes(color = Period), 
                  contour_var = "ndensity",
                  breaks = c(0.25),
                  linewidth = 0.8) +
  geom_segment(data = top_loadings,
               aes(x = 0, y = 0, xend = PC1 * 3, yend = PC2 * 3),
               arrow = arrow(length = unit(0.2, "cm")),
               color = "black", alpha = 0.5, inherit.aes = FALSE) +
  geom_text_repel(aes(label = word, x = PC1 * 3, y = PC2 * 3),
                  data = top_loadings, color = "black",
                  family = getOption("pilot.geom_text_family"),  # Pilot font                            # Pilot style
                  size = 3.5, inherit.aes = FALSE) +
  pilot::theme_pilot(grid = "", axes = 'bl') +
  scale_fill_met_d(name = 'Isfahan1') +
  scale_color_met_d(name = 'Isfahan1') 

ggsave(
  "wordpca.png",
  plot = last_plot(),
  device = NULL,
  path = "figs/fig12/other_vs/",
  scale = 1,
  width = 7,
  height = 5,
  units = c("in", "cm", "mm", "px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
)
