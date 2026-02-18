# Details ----
#' 5_gam_catch.R
#' Paper: [Est. extent of range contraction and extinction timeline]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 2025-06-03
#' Content: 
#' Input: EC_Dec.xlsx
#' Output: GAM results, sup figs, stats for results
#' -----------

source('helpers/help_stat.R')
EC_Dec <- readxl::read_xlsx("data/xls/processed/EC_Dec.xlsx") 

# filter ----
EC_Dec <- EC_Dec %>% 
  mutate(
    Zone = factor(Zone,
                  levels = c('Cape York QLD', 'Far North QLD',
                             'North QLD', 'Central QLD',
                             'Wide Bay QLD', 'South East QLD',
                             'North Coast NSW', 'Mid Coast NSW'))
  ) %>% 
  filter(Zone != "Cape York QLD",
         Zone != "Mid Coast NSW") #remove for low count data

# model of catch freq w/ decade----

gam0 <- gam(Catchfreq ~ s(Decade),
            family = nb,
            data = EC_Dec)
summary(gam0)

gam1 <- gam(Catchfreq ~ s(Decade, by = Zone),
            family = nb,
            data = EC_Dec) 

summary(gam1)
draw(gam1)
anova(gam1, gam0) #sig better

gam2 <- gam(Catchfreq ~ s(Decade) + Zone, 
            family = nb,
            data = EC_Dec) 
summary(gam2)
anova(gam0, gam2) 
anova(gam1, gam2) #adding Zone as main effect better

# make plots----
p1 <- plot_predictions(gam2, by = "Decade", rug = TRUE) +
  labs(y = "Annual Sawfish Encounters") +
  theme_minimal() +
  theme(axis.title.x = element_text(margin = 
                                      margin(t = -30, b = 0)))

p2 <- plot_predictions(gam2, condition = "Zone") +
  labs(y = "", x = "") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p1 + p2 + plot_layout(widths = c(1, 2)) +
  plot_annotation(tag_levels = 'a', tag_suffix = ')')

ggsave(
  "figs/s7/s7a_partialab_plot.tiff",
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

# appraisal plot----
appraise(gam2) #k its that one i think

ggsave(
  "figs/s7/s7a_res_plot.png",
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 6,
  height = 4,
  units = c("in", "cm", "mm", "px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL
)
