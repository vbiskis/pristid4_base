# Details ----
#' 3_summary_stats.R
#' Paper: [Est. extent of range contraction and extinction timeline]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 2025-06-07
#' Content: grab some summary stats for beginning of results section
#' Input: EC_Date.xlsx
#' Output: 5.3 data reports
#' -----------

pacman::p_load('readxl', 'dplyr', 'tidyverse')

filthist <- readxl::read_xlsx("data/xls/processed/filthist.xlsx") # all reports, less duplicates
EC_Spec <- readxl::read_xlsx("data/xls/processed/EC_Spec.xlsx") #just mapping

# useful numbers----
#Start of Section 5.3
## pre-filter looking----
filthist <- filthist %>%
  mutate(
    Species = fct_collapse(Species, #eee bad practice should've done this earlier
                           "A. cuspidata" = c("A. cuspidata", "Anoxypristis cuspidata"),
                           "P. clavata" = c("P. clavata", "Pristis clavata"),
                           "P. pristis" = c("P. pristis", "Pristis pristis"),
                           "P. clavata / P. pristis" = "P. pristis or P. clavata",
                           "P. zijsron" = c("P. zijsron", "Pristis zijsron"),
                           "A. cuspidata / P. zijsron" = "P. zijsron or A. cuspidata",
                           "Pristis sp." = c("Pristis sp.", "Pristis spp.")),
    Spec_Known = if_else(
      Species %in% c('A. cuspidata', 
                     'P. pristis', 
                     'P. clavata', 
                     'P. zijsron', 
                     'Pristiophorus sp.'),
      Species,
      "Pristidae"
    ),
    Confirmation = if_else(Spec_Acc %in% c(1, 2), "Confirmed", "Unconfirmed")
  ) #just whether I could ID spec 

filthist %>%
  count(Spec_Known, 
        Confirmation) %>%
  pivot_wider(names_from = Confirmation, values_from = n, values_fill = 0) %>%
  mutate(Total = Confirmed + Unconfirmed) #all records, even those without date or location.

## all records----
filthist %>% 
  filter(Species == "P. pristis", 
         Medium == "Museum Record" |
         Type == "Published media") %>% 
  group_by(Medium, Type, Division, Spec_Acc, Loc_Acc) %>% 
  summarise(
    n = n()
  ) %>% 
  print(n= Inf)

# filtered (reporting)----
EC_Spec %>% #simple numbers
  filter(Loc_Acc < 4) %>% 
  summarise(
    nbasins = n_distinct(Basin),
    photo_all = n_distinct(Basin[Type_S == "Photograph" | #photo conf
                               (Spec_Acc == 1 & Type_S == "Published media")]),
    photo_inmedia = n_distinct(Basin[Spec_Acc == 1 & Type_S == "Published media"]),
    photo_sawc = n_distinct(Basin[Spec_Acc == 1 & Rec_Acc < 3]), #addl saws
  ) 

EC_Spec %>% #species totals
  group_by(Spec_Known) %>% 
  arrange(Basin, desc(mean_lat)) %>% 
  summarise(
    nbasins = n_distinct(Basin[Rec_Acc %in% c(1:3) |
                                 (Rec_Acc == 0 & Medium == "Museum Record")]),
    basins = paste(unique(Basin), collapse = ", "),
    count = n(),
    saw = sum(Type_S == "Specimen"),
    photo = sum(Type_S == "Photograph" | 
                  (Spec_Acc == 1 & Type_S == "Published media")),
    news = sum(Type_S == "Published media"),
    famphot = sum(Type_S == "Photograph"),
    conf = sum(Rec_Acc %in% c(1, 2), na.rm = TRUE),
    prob = sum(Rec_Acc == 3, na.rm = TRUE),
    report = conf + prob,
    unc_s = sum(Type_S == "Specimen"  & Rec_Acc == 0, na.rm = TRUE),
    unc_n = sum(Type_S == "Published media"  & Rec_Acc == 0, na.rm = TRUE),
    unc_all = sum(Rec_Acc == 0, na.rm = TRUE)
  ) 

EC_Spec %>% #specifics for AC and PC
  filter(Spec_Known %in% c('A. cuspidata', 'P. clavata')) %>% 
  group_by(Spec_Known, Basin) %>% 
  summarise(
    count = n(),
    news = sum(Type_S == "Published media"),
    photo = sum(Type_S == "Photograph" | 
                  (Spec_Acc == 1 & Type_S == "Published media")),
    saw = sum(Type_S == "Specimen"),
    conf = sum(Rec_Acc %in% c(1, 2), na.rm = TRUE),
    prob = sum(Rec_Acc == 3, na.rm = TRUE),
    report = conf + prob,
    unc = sum(Rec_Acc == 0, na.rm = TRUE),
    mean_lat = first(mean_lat),
  ) %>% 
  arrange(Spec_Known, desc(mean_lat)) %>% 
  print(n = Inf)

## 5.3 spatial dist of sighting quality----
EC_Spec %>% #just a check
  filter(Basin == "TORRES STRAIT ISLANDS") %>% 
  group_by(Spec_Known, Rec_Acc, Spec_Acc, Medium_Name) %>% 
  summarise(
    n = n(),
    year = Year
  )

EC_Spec %>% 
  group_by(Zone, Region, Basin) %>% 
  summarise(
    count = n(),
    high = sum(Rec_Acc == 1, na.rm = TRUE),
    med = sum(Rec_Acc == 2, na.rm = TRUE),
    low = sum(Rec_Acc == 3, na.rm = TRUE),
    unc = sum(Rec_Acc == 0, na.rm = TRUE),
    conf = sum(high, med)/count) %>% 
  print(n = Inf)

