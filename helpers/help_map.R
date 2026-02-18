# Details ----
#' help_map.R
#' Paper: [Est. extent of range contraction and extinction timeline]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 2025-11-14
#' Content: General settings for mapping
#'  avoid repetition
#' -----------

pacman::p_load('dplyr', 'tidyverse', 
               'terra', 'sf', 'sp', 
               'rnaturalearth', 'maps',
               'tmap', 'tmaptools', 'devtools',
               'ggspatial', 'ggthemes', 
               'geodata', 'geosphere')

aus_east <- readRDS('data/rds/aus_east.rds') #east coast underlay

studyarea <- ext(c(138, 142, -35, -10))
studybox <- st_bbox(c(xmin = 142, xmax = 156, ymin = -35, ymax = -10))
blankgrid <- st_make_grid(studybox, n = c(1, 1), crs = "EPSG:4326") #no grid

# common mapping calls----
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

spec_all_labels <- c(
  expression(italic("P. clavata")),
  expression(italic("P. pristis")),
  expression(paste(italic("P. clavata"), " or ", italic("P. pristis"))),
  expression(italic("A. cuspidata")), 
  expression(italic("P. zijsron")),
  expression(paste(italic("P. zijsron"), " or ", italic("A. cuspidata"))),
  expression(italic("Pristis sp.")),
  expression("Pristidae")
)

