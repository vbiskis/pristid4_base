# Details ----
#' 1_dfs3_filter.R
#' Paper: [Est. extent of range contraction and extinction timeline]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 2025-05-07
#' Content: apply filters for future figures/stats tests 
#' Input: timeline.xlsx
#' Output: 
#' + spec_mapping (no sawsharks, unk species)
#' + EC_Spec (only east coast)
#' + EC_Date (only known year)
#' -----------

pacman::p_load('readxl', 'writexl', 'sf',
               'tidyverse', 'dplyr', 'tidyr')

timeline <- readxl::read_xlsx("data/xls/processed/timeline.xlsx")

# filter 1: removing unknowns ----
spec_mapping <- timeline %>% 
  dplyr::filter(Spec_Conf < 3, #remove odd ones 
                (Date_Acc > 0 | Medium == "Museum Record"), #date
                !is.na(State)) %>% # already did this earlier 
  mutate(Spec_Known = fct_collapse(Species,
                                   "Pristidae" = c("P. clavata / P. pristis",
                                                   "A. cuspidata / P. zijsron",
                                                   "Pristis sp.", "Pristidae")),
         Type_S = fct_collapse(Type,
                               "Photograph" = c("Photo", "Photo/Cast", "Photo/Saw"),
                               "Specimen" = c("Saw", "Whole specimen", "Saw, Indigenous artwork")))

spec_mapping$Spec_Known <- fct_relevel(spec_mapping$Spec_Known, 
                                       "A. cuspidata", "P. clavata", "P. pristis", "P. zijsron", "Pristidae")
#want pristidae last
spec_mapping$Type_S <- fct_relevel(spec_mapping$Type_S, 
                                   "Photograph", "Published media", "Specimen", "Encounter")

#filter 2: east coast only----
EC_Spec <- spec_mapping %>% #only confirmed on EC
  dplyr::filter(Division != "GULF OF CARPENTARIA") %>% 
  mutate(Region2 = case_when(Basin == "JOHNSTONE RIVER" | Basin == "MURRAY RIVER" |
                               Basin == "HERBERT RIVER" | Basin == "HINCHINBROOK ISLAND" |
                               Basin == "TULLY RIVER" ~ "JOHNSTONE",
                             Basin == "BLACK RIVER" | Basin == "ROSS RIVER" ~ "BURDEKIN",
                             Basin == "BAFFLE CREEK" ~ "BURNETT",
                             Basin == "MAROOCHY RIVER" ~ "BRISBANE",
                             Basin == "TWEED RIVER" ~ "GOLD COAST",
                             Basin == "BRUNSWICK RIVER" | Basin == "RICHMOND RIVER" |
                               Basin == "CLARENCE RIVER" ~ "NORTHERN RIVERS",
                             Basin == "BELLINGER RIVER" | Basin == "MACLEAY RIVER" |
                               Basin == "HASTINGS RIVER" | Basin == "MANNING RIVER" ~ "COFFS COAST",
                             .default = Region)) %>% 
  dplyr::select(1:9, 38, everything())

EC_Spec$Spec_Known <- droplevels(EC_Spec$Spec_Known)
EC_Spec$Species <- droplevels(EC_Spec$Species)

# filter 3: confirmed years only----
## get in basin names----
ecbasins <- readRDS('data/rds/ecbasins.rds')

## make a table----
## ... for the boat times

basin_table <- ecbasins %>%
  st_drop_geometry() %>%
  mutate(
    region_name = case_when(
      CENTROID_Y > -16 ~ "Cape York QLD",
      (CENTROID_Y > -18.5) ~ "Far North QLD", 
      (CENTROID_Y > -21.5) ~ "North QLD", 
      CENTROID_Y > -24 ~ "Central QLD",
      CENTROID_Y > -26.5 ~ "Wide Bay QLD",
      (CENTROID_Y > -28.45) ~ "South East QLD",
      CENTROID_Y > -32 ~ "North Coast NSW",
      CENTROID_Y > -34.5 ~ "Mid Coast NSW",
      TRUE ~ "South Coast NSW"
    )
  ) %>%
  group_by(BNAME) %>%
  slice(1) %>%  # Just take the first occurrence of each basin name
  ungroup() %>%
  dplyr::select(region_name, RNAME2, BNAME) %>%
  arrange(match(region_name, c("Cape York QLD", "Far North QLD", "North QLD", 
                               "Central QLD", "Wide Bay QLD", "South East QLD", 
                               "North Coast NSW", "Mid Coast NSW", "South Coast NSW")))

## merge----
EC_Spec <- EC_Spec %>% 
  left_join(basin_table %>%
              dplyr::select(-BNAME) %>% 
              distinct(RNAME2, .keep_all = TRUE), 
            by = c("Region2" = "RNAME2")) %>% 
  mutate(RegionOld = Region,
         mean_lat = case_when(Basin == "LOGAN-ALBERT RIVERS" ~ -27.5,
                              Basin == "STRADBROKE ISLAND" ~ -27.6,
                              Basin == "BRISBANE RIVER" ~ -27.4,
                              .default = mean_lat),
         Region = fct_reorder(Region2, mean_lat),
         Zone = fct_reorder(region_name, mean_lat)) %>% 
  dplyr::select(1:4, Basin, Region, Zone, everything(), -region_name, -Region2) 

EC_Spec$Region <- droplevels(EC_Spec$Region)

## last edits----
EC_Spec <- EC_Spec %>% 
  filter(Year <= 1990 | is.na(Year),
         !is.na(Latitude)) %>% 
  mutate(Rec_Acc = case_when(Date_Acc <= 2 & Date_Acc > 0 & Spec_Acc > 0 & Spec_Acc <= 2 & Loc_Acc > 0 & Loc_Acc < 3 & 
                               (Type_S %in% c("Photograph", "Published media") |  
                                  Medium %in% c("Museum Record", "Display")) ~ 1,
                             (Date_Acc <= 3 & Date_Acc > 0 & Spec_Acc > 0 & Loc_Acc > 0 & Loc_Acc <= 3) & 
                               (Type_S %in% c("Photograph", "Published media") | 
                                  Medium %in% c("Museum Record", "Display")) ~ 2,
                             (Date_Acc > 0 & Loc_Acc > 0) & #must not be unknown
                               (Date_Acc <= 3 & Loc_Acc <= 3 & Type_S == "Published media") | # likely news that with little desc./no photo
                               (Date_Acc <= 3 & Loc_Acc <= 3 & #high quality personal accounts
                                  Spec_Acc > 0 & Type_S %in% c("Encounter", "Specimen")) ~ 3, 
                             .default = 0)
  ) %>% 
  dplyr::select(ID, Rec_Acc, Spec_Acc, Date_Acc, Loc_Acc, Medium, Type_S, everything()) #looks good!


EC_Spec <- EC_Spec %>% 
  mutate(Basin = case_when(is.na(Basin) & Region == "BRISBANE" 
                           & Medium == "Museum Record" ~ "BRISBANE RIVER",
                           .default = Basin),
         mean_lat = case_when(Basin == "BRISBANE RIVER" ~ -27.40000,
                              .default = mean_lat))
## filter----
EC_Date <- EC_Spec %>% 
  filter(Date_Acc > 0 & Date_Acc < 4) #keep 0.5! super accurate!

#final dfs----
write_xlsx(spec_mapping, "data/xls/processed/spec_mapping.xlsx") # removes all unknown species or sawsharks
write_xlsx(EC_Spec, "data/xls/processed/EC_Spec.xlsx") # on only east coast
write_xlsx(EC_Date, "data/xls/processed/EC_Date.xlsx") # with no unknown years

