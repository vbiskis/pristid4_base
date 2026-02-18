# Details ----
#' 2_fig4_studyarea.R
#' Paper: [Est. extent of range contraction and extinction timeline]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 2025-06-01
#' Content: overlay basins on east coast map
#' Dependencies: 
#'  + mapping src code
#'  + 2x rds files prev made (1_set_maps.R)
#' Output: methods study area figure (fig4.png/tiff)
#' -----------

source('helpers/help_map.R')
ecbasins <- readRDS('data/rds/ecbasins.rds') #overlay

# need a couple visuals here
## align labels at eastern most point
## boxes around regions

#find eastern boundary----
##get points----
coast_points <- ecbasins %>%
  st_cast("POINT") %>%
  mutate(
    lon = st_coordinates(.)[,1],
    lat = st_coordinates(.)[,2]
  ) %>%
  st_drop_geometry()

##call the most eastern----
east_boundary <- function(target_lat, tolerance = 0.5) {
  nearby_points <- coast_points %>%
    filter(abs(lat - target_lat) <= tolerance)
  
  if(nrow(nearby_points) > 0) {
    return(max(nearby_points$lon))
  } else {
    return(max(coast_points$lon))  # Fallback to overall easternmost point
  }
}

# make the boxes----
## a function to bound those regions----
make_box <- function(west, east, south, north) {
  st_polygon(list(matrix(c(
    west, south,
    east, south,
    east, north,
    west, north,
    west, south
  ), ncol = 2, byrow = TRUE)))
}

## name em----
regions_coastal <- tibble(
  region_name = c("South Coast NSW", "Mid Coast NSW", "North Coast NSW", "South East QLD",
                  "Wide Bay QLD", "Central QLD", "North QLD", "Far North QLD", "Cape York QLD"),
  lat_south = c(-37.75, -34.75, -32, -28.5, -26.5, -24, -21.25, -18.75, -15.75),
  lat_north = c(-34.75, -32, -28.5, -26.5, -24, -21.25, -18.75, -15.75, -11)
) %>%
  mutate(
    coastline_south = map_dbl(lat_south, east_boundary),  # Coastline at southern boundary
    coastline_north = map_dbl(lat_north, east_boundary),  # Coastline at northern boundary
    west_bound = pmin(coastline_south, coastline_north) - 0.75,  # get it to hide behind state border
    east_bound = 161  # Fixed eastern boundary for box 
  ) %>%
  rowwise() %>%
  mutate(
    geometry = list(make_box(west_bound, east_bound, lat_south, lat_north))
  ) %>%
  st_as_sf(crs = st_crs(ecbasins))

# And plot!----
studyplace <- ggplot() +
  geom_sf(data = regions_coastal, fill = NA, color = "grey", size = 1) +
  geom_sf(data = aus_east, fill = "white") +
  geom_sf(data = ecbasins, aes(fill = RNAME2), color = "black", size = 0.2) +
  geom_sf_text(data = regions_coastal, aes(label = region_name), 
               x = regions_coastal$east_bound - 3.5, size = 3.5) +
  scale_fill_viridis_d(
    name = "River Region",
    option = "turbo",
    direction = -1 
  ) +
  theme_void()

ggsave(
  "fig4.png",
  plot = studyplace,
  device = NULL,
  path = 'figs/fig4/',
  scale = 1,
  width = 6,
  height = 6,
  units = c("in", "cm", "mm", "px"),
  #dpi = 300,
  limitsize = TRUE,
  bg = NULL,
)
