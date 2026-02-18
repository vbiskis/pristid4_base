# Details ----
#' 1_set_maps.R
#' Paper: [Est. extent of range contraction and extinction timeline]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 2025-06-01
#' - Split to sep script 2025-11-14
#' Content: Create basemaps for use in figs
#' Output: 
#' + aus_east.rds
#' + ecbasins.rds
#' -----------

pacman::p_load('dplyr', 'tidyverse', 
               'terra', 'sf', 'sp', 
               'rnaturalearth', 'maps',
               'tmap', 'tmaptools', 'devtools',
               'ggspatial', 'ggthemes', 
               'geodata', 'geosphere')

#make east coast map----
aus <- ne_countries(scale = "medium", country = "Australia", returnclass = "sf") %>%
  st_transform(crs = 4202)  # Transform to match your basins

# Create bounding box in the same CRS
fullbox <- st_polygon(list(matrix(c(
  142, -38,
  156, -38,
  156, -10,
  142, -10,
  142, -38
), ncol = 2, byrow = TRUE))) %>%
  st_sfc(crs = 4202)  # Set CRS to 4202

aus_east <- st_intersection(st_geometry(aus), fullbox)
plot(aus_east) #looks good

saveRDS(aus_east, 'data/rds/aus_east.rds')

#make rivers map----
catch <- st_read('data/maps/aus_basins/rbasin_polygon.shp')
head(catch)
ecbasins <- catch %>% 
  filter(DNAME %in% c('NORTH-EAST COAST', 'SOUTH-EAST COAST'),
         !(CENTROID_X < 149 & CENTROID_Y < -36.5)) %>% 
  mutate(BNAME = fct_reorder(BNAME, CENTROID_Y, .desc = TRUE),
         RNAME2 = case_when(BNAME == "JOHNSTONE RIVER" | BNAME == "MURRAY RIVER (QLD)" |
                              BNAME == "HERBERT RIVER" | BNAME == "HINCHINBROOK ISLAND" |
                              BNAME == "TULLY RIVER" ~ "JOHNSTONE",
                            BNAME == "BLACK RIVER" | BNAME == "ROSS RIVER" ~ "BURDEKIN",
                            (RNAME == "CURTIS" & BNAME != "BAFFLE CREEK") | RNAME == "FITZROY (QLD)" ~ "FITZROY",
                            BNAME == "BAFFLE CREEK" ~ "BURNETT",
                            BNAME == "MAROOCHY RIVER" ~ "BRISBANE",
                            BNAME == "TWEED RIVER" ~ "GOLD COAST",
                            BNAME == "BRUNSWICK RIVER" | BNAME == "RICHMOND RIVER" |
                              BNAME == "CLARENCE RIVER" ~ "NORTHERN RIVERS",
                            BNAME == "BELLINGER RIVER" | BNAME == "MACLEAY RIVER" |
                              BNAME == "HASTINGS RIVER" | BNAME == "MANNING RIVER" ~ "COFFS COAST",
                            .default = RNAME),
         RNAME2 = fct_reorder(RNAME2, CENTROID_Y, .desc = TRUE))

saveRDS(ecbasins, 'data/rds/ecbasins.rds') #and save you for later too!

#check----
ggplot() +
  geom_sf(data = ecbasins, 
          aes(fill = RNAME2), color = "black") #nice!

