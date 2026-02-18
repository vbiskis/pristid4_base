# Details ----
#' help_stat.R
#' Paper: [Est. extent of range contraction and extinction timeline]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 2025-11-14
#' Content: General settings for stat tests
#'  avoid repetition
#' -----------

pacman::p_load('ggplot2', 'tidyverse', 'tidyr', 'dplyr', 
               'magrittr', 'GGally',
               'ggfortify', 'ggpubr', 'ggmosaic',
               'patchwork', 'kableExtra', 'grid',
               'mgcv', 'e1071', 'MASS', 'performance', 'vcd',
               'DHARMa', 'gstat', 'marginaleffects', 'gratia',
               'partykit', 'stats', 'vegan', 
               'car', 'psych', 'boot')

species_colors <- c(
  "A. cuspidata" = "blue4",
  "P. clavata" = "gold2",
  "P. pristis" = "red3",
  "P. zijsron" = "darkgreen",
  "Pristidae" = "grey50"
)

species_labels <- c(
  expression(italic("A. cuspidata")), 
  expression(italic("P. clavata")),
  expression(italic("P. pristis")),
  expression(italic("P. zijsron")),
  expression("Pristidae")
)
