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
source('helpers/help_map.R')
set_theme(mytheme)

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
  theme(axis.title.x = element_text(vjust = 15),
        axis.text.x = element_text(vjust = 0.8, margin = margin(b = 0)))

p2 <- plot_predictions(gam2, condition = "Zone") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1,
                                   margin = margin(b = 0)),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

p1 + p2 + plot_layout(widths = c(1, 2)) +
  plot_annotation(tag_levels = 'a', tag_suffix = ')') &
  theme(plot.margin = margin(5, 5, 0, 5),
        plot.tag = element_text(margin = margin(b = -10, r = -15),
                                size = 13, family = "optima"))

ggsave(
  "s7a_parplot_ab.png",
  plot = last_plot(),
  device = NULL,
  path = "figs/s7/",
  scale = 1,
  width = 6,
  height = 4,
  units = c("in", "cm", "mm", "px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
)

# appraisal plot----
appraise(gam2) & theme(plot.title = element_text(size = 12, family = "Optima",
                                                 margin = margin(t = 5, b = 10, unit = "pt")),
                       plot.subtitle = element_text(margin = margin(b = 5, unit = "pt")),
                       plot.margin = margin(l = 2, t = 0, b = 5, r = 10, unit = "pt"),
                       axis.title.y = element_text(margin = margin(r = 5, unit = "pt")))
  
ggsave(
  "s7b_res_plot.png",
  plot = last_plot(),
  device = NULL,
  path = 'figs/s7/',
  scale = 1,
  width = 6,
  height = 4,
  units = c("in", "cm", "mm", "px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL
)
