# Details ----
#' 4_spec_comp_basin.R
#' Paper: [Est. extent of range contraction and extinction timeline]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 2025-05-28
#' Content: species composition across basins
#' Input: EC_Date.xlsx
#' Output: Fig 5 - species bar plot
#' -----------

source('helpers/help_map.R')
EC_Spec <- readxl::read_xlsx("data/xls/processed/EC_Spec.xlsx") #just mapping!

source('helpers/help_news.R')
set_theme(mytheme)

#frequency of reports----
spec_basin <- EC_Spec %>%
  filter(Rec_Acc > 0, 
         Basin != "WOLLONGONG COAST", #outside conf.
         !is.na(Basin)) %>% 
  mutate(Basinnames = case_when(Basin == "SYDNEY COAST-GEORGES RIVER" ~ "SYDNEY COAST",
                                Basin == "MACQUARIE-TUGGERAH LAKES" ~ "MQ-TUGGAERAH",
                                Basin == "HAWKESBURY RIVER" ~ "HAWKESBURY",
                                TRUE ~ as.character(Basin)),
         Basinnames = fct_reorder(Basinnames, mean_lat),
         Acc = case_when(
           Rec_Acc %in% c(1, 2) ~ "Confirmed", 
           Rec_Acc == 3 ~ "Unverified"))

spec_basin <- spec_basin %>% 
  mutate(Acc = factor(Acc, 
                         levels = c("Unverified", "Confirmed")))

zone_breaks <- spec_basin %>%
  arrange(Basinnames) %>%
  mutate(basin_num = as.numeric(Basinnames)) %>%
  group_by(Zone) %>%
  summarise(max_basin = max(basin_num), .groups = 'drop') %>%
  slice(-n()) %>%  # Remove the last one
  pull(max_basin) + 0.5

species_ranges <- data.frame(
  species = c("A. cuspidata", "P. clavata", "P. pristis", "P. zijsron"),
  start_basin = c("NOOSA RIVER", "JACKY JACKY CREEK", "NORMANBY RIVER", "SHOALWATER CREEK"),
  colors = species_colors[1:4]  # Assuming first 4 colors correspond to these species
)

# Get the number of basins for the right edge
n_basins <- length(unique(spec_basin$Basinnames))

# Create the plot
ggplot(spec_basin) +
  geom_bar(aes(x = Basinnames, fill = Spec_Known,
               color = ifelse(Acc == "Confirmed", 
                              "black",
                              species_colors[Spec_Known]),
               alpha = ifelse(Acc == "Unverified", 0.5, 1)),
           position = "stack") +
  scale_color_identity() +  
  scale_alpha_identity() +   
  # Horizontal range bars for each species
  geom_segment(data = species_ranges,
               aes(x = start_basin, xend = n_basins + 0.4,
                   y = c(40, 38, 36, 34), yend = c(40, 38, 36, 34)),
               color = species_ranges$colors,
               linewidth = 4,
               alpha = 0.75,
               inherit.aes = FALSE) +
  # Vertical zone lines
  geom_vline(xintercept = zone_breaks, color = "black", 
             linetype = "longdash", linewidth = .25,
             alpha = 0.7, lineend = "round") +
  scale_fill_manual(name = "Species", 
                    values = species_colors,
                    labels = species_labels) +
  scale_x_discrete(breaks = levels(spec_basin$Basinnames)[c(TRUE, FALSE)]) +
  labs(y = "Total Records", x = "River Basin") +
  theme(axis.text.x = element_text(angle = 90, size = 12, hjust = 1),
        axis.title = element_text(size = 13),
        legend.text = element_text(size = 12), legend.title = element_text(size = 13),
        plot.margin = margin(2, 2, 2, 2, "pt"))

ggsave(
  "fig5.png",
  plot = last_plot(),
  device = NULL,
  path = "figs/fig5/",
  scale = 1,
  width = 10,
  height = 5,
  units = c("in", "cm", "mm", "px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
)


