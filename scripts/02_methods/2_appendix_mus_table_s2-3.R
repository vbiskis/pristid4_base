# Details ----
#' 2_appendix_mus_table.R
#' Paper: [Est. extent of range contraction and extinction timeline]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 2025-11-20
#' Content: for supps: table of all museum sources
#' Output: 
#' + appendix table - specimen details (app_s2.docx)
#' + appendix table - access summary (app_s3.docx)

#' -----------

pacman::p_load("ggplot2", 
               "dplyr",
               "readxl",
               "flextable")

#want all dates for full visual
EC_Spec <- readxl::read_xlsx("data/xls/processed/EC_Spec.xlsx") 
museumdets <- readxl::read_xlsx("data/xls/Museum_Saws.xlsx", sheet = 'Contact_History') #for museum citation

# final table of institutions to cite----
source_table <- allhist %>% 
  filter(Medium %in% c('Museum Record', 'Display'),
         Mode != "Fishing Shack",
         Division != c('GULF OF CARPENTARIA'),
         Year < 1990 | is.na(Year),
         State %in% c('QLD', 'NSW')) %>% 
  left_join(museumdets %>% 
              select(In_Type, In_Pub, In_Name, MusAbbrev, Region, State) %>% 
              rename(RegionM = Region,
                     StateM = State), by = c("Medium_Name" = "In_Name")) %>%  # adjust join key as needed
  mutate(
    # Add flag for where associated news article
    FoundNews = !is.na(ID2) & grepl("TRV", ID2, ignore.case = TRUE),
    ) %>%
  select(
    ID, Species, Type,
    River, Basin, #Loc_Acc,
    Year, #Date_Acc,
    TL, #Sex, 
    MusAbbrev, RegionM, In_Type, In_Pub,
    ID2
  ) %>%
  mutate(Year = as.character(Year),
         In_Pub = factor(In_Pub, levels = c("Metro", "Regional", "Public", "Private")),
         In_Type = factor(In_Type, levels = c("Museum", "Library", "Aquarium", "Research/Education",
                                              "Government", "Fishing Club", "Hotel/Pub")),
         RegionM = factor(RegionM, levels = c("BARRON", "SHOALWATER BAY", "BURNETT",
                                              "MARY", "BRISBANE", "GOLD COAST", "COFFS HARBOUR",
                                               "SYDNEY", "DOMESTIC", "INTERNATIONAL")),
         Species = fct_collapse(Species,
                                "A. cuspidata" = c("A. cuspidata", "Anoxypristis cuspidata"),
                                "P. clavata" = c("P. clavata", "Pristis clavata"),
                                "P. pristis" = c("P. pristis", "Pristis pristis"),
                                "P. zijsron" = c("P. zijsron", "Pristis zijsron")),
         TL = round(TL, 0)
  ) %>%  #otherwise adds an annoying comma!
  arrange(In_Pub, RegionM, MusAbbrev) %>%
  distinct()

ids_used <- source_table %>%
  filter(ID %in% EC_Date$ID | FoundNews == TRUE) 

#need to fix order! for reprint----
ids_used %>% 
  print(n = Inf)

ids_used %>%
  flextable() %>%
  save_as_docx(path = "figs/s2/source_table.docx")

## all museums details----
allhist %>% 
  filter(Medium %in% c('Museum Record', 'Display'),
         Mode != "Fishing Shack",
         #Division != c('GULF OF CARPENTARIA'),
         Year < 1990 | is.na(Year)) %>% 
  left_join(museumdets %>% 
              select(In_Type, In_Name, In_Pub, MusAbbrev, Region, State) %>% 
              rename(RegionM = Region,
                     StateM = State), by = c("Medium_Name" = "In_Name")) %>%  # adjust join key as needed
  select(
    ID, Type, Division,
    Latitude, Year,  
    MusAbbrev, Medium_Name,
    RegionM, In_Type, In_Pub
  ) %>% 
  mutate(In_Pub = factor(In_Pub, levels = c("Metro", "Regional", "Public", "Private")),
         In_Type = factor(In_Type, levels = c("Museum", "Library", "Aquarium", "Research/Education",
                                              "Government", "Fishing Club", "Hotel/Pub")),
         RegionM = factor(RegionM, levels = c("PRINCESS CHARLOTTE BAY", "WEIPA", "MITCHELL", "GILBERT", "BURKETOWN", 
                                              "BARRON", "SHOALWATER BAY", "BURNETT",
                                              "MARY", "BRISBANE", "GOLD COAST", "COFFS HARBOUR",
                                              "HUNTER", "SYDNEY", "DOMESTIC", "INTERNATIONAL"))) %>% 
  group_by(In_Pub, RegionM, In_Type, MusAbbrev, Type, Medium_Name) %>% 
  summarise(
    n = n(),
    goc = sum(Division == "GULF OF CARPENTARIA", na.rm = TRUE),
    nayr = sum(is.na(Year)),
    naloc = sum(is.na(Latitude)),
    totused = sum(!is.na(Year) & !is.na(Latitude) & Division != "GULF OF CARPENTARIA")
  ) %>% 
  #print(n = Inf) %>%  
  flextable() %>%
  save_as_docx(path = "figs/s3/source_tableb.docx")

