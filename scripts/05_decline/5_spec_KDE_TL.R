# Details ----
#' 5_spec_kde.R
#' Paper: [Est. extent of range contraction and extinction timeline]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 2025-06-04
#' Content: watching the spatial fragmentation of sawfishes
#' Input: EC_Date.xlsx
#' Output: map/kdes over time
#' + EC_Dens: only verified accounts, east coast, with a period col in.
#' -----------

pacman::p_load('readxl', 'writexl', 
               'tidyverse', 'dplyr', 'tidyr')

source('helpers/help_map.R')
source('data/rds/ecbasins.rds') #sometimes need to rerun whole script :(

EC_Date <- readxl::read_xlsx("data/xls/processed/EC_Date.xlsx") 

## max min etc----
EC_Date %>% 
  group_by(Spec_Known) %>% 
  summarise(
    n = n(),
    meanTL = mean(TL, na.rm = TRUE),
    basins = n_distinct(Basin),
    regions = n_distinct(Region),
    zones = n_distinct(Zone),
    minyear = min(Year, na.rm = TRUE),
    maxyear = max(Year, na.rm = TRUE)
  )

# a very cool graph----
EC_Dens <- EC_Date %>%
  filter(Year > 1850,
         Rec_Acc %in% c(1, 2, 3)) %>% 
  group_by(Region, Basin) %>%
  mutate(
    count = n(),
    Basin_plot = if_else(count <= 2, NA_character_, as.character(Basin))
  ) %>%
  ungroup() %>% 
  dplyr::select(-count) %>% 
  mutate(Region = fct_reorder(Region, mean_lat),
         Basin_plot = fct_reorder(Basin_plot, mean_lat),
         Zone = fct_reorder(Zone, mean_lat))

EC_Dens <- EC_Dens %>% 
  mutate(Period = case_when(Year <= 1900 ~ "1900", #pre-Federation
                            Year > 1900 & Year <= 1915 ~ "1901-1915", #pre WWI
                            Year > 1915 & Year <= 1930 ~ "1916-1930", #post WWI
                            Year > 1930 & Year <= 1945 ~ "1931-1945", #pre WWII
                            Year > 1945 & Year <= 1960 ~ "1946-1960", #post WWII
                            Year > 1960 & Year <= 1975 ~ "1961-1975", #modern fishing
                            Year > 1975 ~ "1976", #pre-conservation
                            .default = NA))

ggplot() +
  geom_sf(data = ecbasins, fill = NA, color = "grey", size = 1) +
  geom_sf(data = aus_east, fill = "white", color = "gray70") +  # Added border color for clarity
  stat_density_2d(data = EC_Dens, 
                  aes(x = Longitude, y = Latitude, color = after_stat(level)),
                  bins = 10, size = 0.75, h = c(2, 2), n = 150) +
  facet_wrap(~ Period, ncol = 5) +  # Removed data and aes arguments
  coord_sf(xlim = c(min(EC_Dens$Longitude, na.rm = TRUE) - 1, 
                    max(EC_Dens$Longitude, na.rm = TRUE) + 1),
           ylim = c(min(EC_Dens$Latitude, na.rm = TRUE) - 1, 
                    max(EC_Dens$Latitude, na.rm = TRUE) + 1)) +  # Better for sf objects than coord_fixed()
  scale_color_viridis_c(name = "Density") +
  labs(title = "",
       x = "Longitude", 
       y = "Latitude") +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    legend.position = "bottom"
  )

write_xlsx(EC_Dens, 'data/xls/processed/EC_Dens.xlsx') #now filtered for probable-verified only

#it's pretty sick hey
#overlay with a population heatmap??