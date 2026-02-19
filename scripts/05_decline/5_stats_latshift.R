# Details ----
#' 5_stats_latshift.R
#' Paper: [Est. extent of range contraction and extinction timeline]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 2025-06-03
#' Content: 
#' Input: EC_Dens.xlsx
#' Output: 
#' -----------

source('helpers/help_stat.R')
source('helpers/help_map.R')
set_theme(mytheme)

EC_Dens <- readxl::read_xlsx("data/xls/processed/EC_Dens.xlsx") 

yearlat <- lm(Year ~ Latitude + Spec_Known, data = EC_Dens)
summary(yearlat) # nah not spec

model1 <- lm(Latitude ~ Year, data = EC_Dens)
summary(model1) # this is the one I report!

EC_Dens <- EC_Dens %>% 
  filter(Zone != "Cape York QLD")

summary(aov(Year ~ Region, data = EC_Dens))
summary(aov(Year ~ Zone, data = EC_Dens))

#Northward Shift----
ggplot(EC_Dens, 
       aes(x = Year, y = Latitude)) +
  geom_point(aes(color = Species, shape = Species,
                 size = Species, alpha = Species), stroke = 1.25) +
  geom_smooth(color = "black", method = "lm") +
  scale_size_continuous(
    name = "Total Length (cm)",
  ) +
  scale_color_manual(name = "Species", 
                     values = c(
                       "A. cuspidata" = "blue4",
                       "P. clavata" = "gold2",
                       "P. pristis" = "red3",
                       "P. clavata / P. pristis" = "orange",
                       "P. zijsron" = "darkgreen",
                       "A. cuspidata / P. zijsron" = "cyan4",
                       "Pristis sp." = "magenta4",
                       "Pristidae" = "grey50"
                     ),
                     labels = spec_all_labels) +
  scale_shape_manual(name = "Species", 
                     values = c(
                       "A. cuspidata" = 4,
                       "P. clavata" = 17,
                       "P. pristis" = 15,
                       "P. clavata / P. pristis" = 14,
                       "P. zijsron" = 16,
                       "A. cuspidata / P. zijsron" = 13,
                       "Pristis sp." = 5,
                       "Pristidae" = 5
                     ),
                     labels = spec_all_labels) +
  scale_size_manual(name = "Species", 
                    values = c(2,2,2,1.5,2,2,1.5,1.5),
                    labels = spec_all_labels) +
  scale_alpha_manual(name = "Species", 
                     values = c(1,1,1,1,1,1,0.3,0.3),
                     labels = spec_all_labels) +
  labs(
    title = "",
    x = "Year",
    y = "Latitude"
  ) +
  theme(
    axis.text = element_text(size = 11),
    legend.position = "bottom",
    legend.box = "horizontal",
    plot.margin = margin(5, 5, 5, 5)
  ) +
  guides(
    color = guide_legend(override.aes = list(size = 4, alpha = 1, stroke = 1)))

ggsave(
  "s6_latitude.tiff",
  plot = last_plot(),
  device = NULL,
  path = "figs/s6/",
  scale = 1,
  width = 8,
  height = 5,
  units = c("in", "cm", "mm", "px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
)

