# Details ----
#' 4_spec_mapping.R
#' Paper: [Est. extent of range contraction and extinction timeline]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 2025-05-24
#' Content: build visual of species maps (jesis look at greens)
#' Input: EC_Spec.xlsx
#' Output: fig6_spec_maps.png
#' -----------

EC_Spec <- readxl::read_xlsx("data/xls/processed/EC_Spec.xlsx") 
source('helpers/help_map.R')

source('helpers/help_news.R')
set_theme(mytheme)

EC_Map <- EC_Spec %>% 
  filter(Rec_Acc > 0)

# EC_Map <- EC_Map %>% 
#   filter(Rec_Acc < 3) #toggle for AC/PC

# hey shapes!!----
# map all----
allcoords <- cbind(EC_Map$Spec_Known, EC_Map$Latitude, EC_Map$Longitude)
head(allcoords)
pts.sp <- as.data.frame(allcoords) 
head(pts.sp)
pts.sp <- st_as_sf(pts.sp, coords = c("V3", "V2")) 
st_crs(pts.sp) <- st_crs("+proj=longlat +datum=WGS84")
pts.sp
xy <- st_coordinates(pts.sp)
pts_sf <- st_transform(pts.sp, crs = 4326)

#some calcs again like Ch2----
# list of species
sp <- unique(pts.sp$V1) #alphabetical

maxD <- vector(length=length(sp))
for (s in 1:length(sp)) {
  # get the coordinates for species 's'
  p <- xy[pts.sp$V1 == sp[s], ]
  # distance matrix
  d <- as.matrix(dist(p))
  # ignore the distance of a point to itself
  diag(d) <- NA
  # get max value
  maxD[s] <- max(d, na.rm=TRUE)
}

maxD_species <- data.frame(species = sp, max_distance = maxD)
maxD_species 

# mapping----

allcoords <- cbind(EC_Map$Spec_Known, EC_Map$Latitude, EC_Map$Longitude)
head(allcoords)
pts.sp <- as.data.frame(allcoords) 
head(pts.sp)
pts.sp <- st_as_sf(pts.sp, coords = c("V3", "V2")) 
st_crs(pts.sp) <- st_crs("+proj=longlat +datum=WGS84")
pts.sp
xy <- st_coordinates(pts.sp)
pts_sf <- st_transform(pts.sp, crs = 4326)

pts_sf <- st_transform(pts.sp, crs = 4326)
p_sp <- as(pts_sf, "Spatial")

pts_sf <- pts_sf %>%
  mutate(V1 = case_when(
    V1 == 1 ~ "A. cuspidata",
    V1 == 2 ~ "P. clavata", 
    V1 == 3 ~ "P. pristis",
    V1 == 4 ~ "Pristidae",
    V1 == 5 ~ "P. zijsron",
    TRUE ~ as.character(V1)  # keep original value if no match
  ))

pts_sf <- pts_sf %>% 
  mutate(facet_group = case_when(
    pts_sf$V1 %in% c("A. cuspidata", "P. clavata") ~ "c)",
    pts_sf$V1 %in% c("P. pristis") ~ "a)", 
    pts_sf$V1 %in% c("P. zijsron") ~ "b)",
    TRUE ~ "d)"))

ggplot() +
  geom_sf(data = aus_east, fill = "white", color = "black") +
  geom_sf(data = pts_sf, aes(colour = factor(V1)), show.legend = TRUE) + 
  facet_wrap(~facet_group, nrow = 1) +
  labs(x = "Longitude (°E)", y = "Latitude (°S)", color = "Species") +
  scale_color_manual(name = "Species", values = species_colors,
                     labels = c(
                       expression(italic("A. cuspidata")), 
                       expression(italic("P. clavata")),
                       expression(italic("P. pristis")),
                       expression(italic("P. zijsron")),
                       expression("Pristidae")
                     )) + 
  scale_x_continuous(breaks = seq(142, 154, by = 5)) +
  scale_fill_manual(name = "Species", values = species_colors, guide = "none") +
  theme(legend.position = "none",
        strip.text = element_text(hjust = 0, size = 12, face = "plain"),
        plot.margin = margin(5, 5, 5 ,5))

ggsave(
  "fig6.png",
  plot = last_plot(),
  device = NULL,
  path = "figs/fig6/",
  scale = 1,
  width = 10,
  height = 6,
  units = c("in", "cm", "mm", "px"),
  #dpi = 300,
  limitsize = TRUE,
  bg = NULL,
)
