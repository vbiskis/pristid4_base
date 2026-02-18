# Details ----
#' 1_dfs2_clean.R
#' Paper: [Est. extent of range contraction and extinction timeline]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 2025-05-07
#' Content: 
#' + check for duplicates 
#' + rename some ugly text
#' + apply first round of filters for gen use across all tests
#' Input: allhist.xlsx
#' Output: timeline.xlsx
#' -----------

pacman::p_load('readxl', 'writexl', 
               'tidyverse', 'dplyr', 'tidyr')

filthist <- readxl::read_xlsx("data/xls/processed/filthist.xlsx") #saws

# let's filter first----
aushist <- filthist %>% 
  filter((State == "QLD" | State == "NSW")) # just to clip out the ones we really don't want
  
# k now check for duplicates again just in case----
dupcheck1 <- aushist %>%
  filter(!is.na(Year)) %>% 
  mutate(
    # Duplicates by River_Basin + Date
    dup_basin = duplicated(paste(Basin, Month, Year)) |
      duplicated(paste(Basin, Month, Year), fromLast = TRUE),
  ) %>% 
  filter(dup_basin) %>% 
  dplyr::select(ID, Species, Basin, Day, Month, Year, TL, Fisher, Medium, Type)

#thats fine - checks out

#we've got some ugliness going on though!

# clean up grammar/spelling----
aushist <- aushist %>% 
  mutate(across(c(Sex, Method_Cap, Fate, Saw_Kept), 
                str_to_lower)) #good start

aushist %>% filter(Sex == "m", 
                   is.na(Num_Pups)) #check it - all good

## that capture method column - ugh!----
aushist <- aushist %>%
  mutate(
    Cap_Clean = case_when(
      str_detect(Method_Cap, regex("line|wire")) ~ "line",
      str_detect(Method_Cap, regex("cast")) ~ "cast net",
      str_detect(Method_Cap, regex("trap")) ~ "fish trap",
      str_detect(Method_Cap, regex("gill|stake|set|shark")) ~ "set net",
      str_detect(Method_Cap, regex("haul|shore")) ~ "seine net, shore",
      str_detect(Method_Cap, regex("seine|mesh|ring|boat")) ~ "seine net, boat",
      str_detect(Method_Cap, "trawl") ~ "trawl",
      str_detect(Method_Cap, regex("drag|prawn")) ~ "drag net",
      str_detect(Method_Cap, "net") ~ "net, general",
      str_detect(Method_Cap, regex("harpoon|spear|toma|shot")) ~ "stabbed/shot",
      str_detect(Method_Cap, regex("rope|lasso|tow|jump")) ~ "roped", 
      str_detect(Method_Cap, regex("wash|stranded|predated")) ~ "stranded/predated",
      str_detect(Fate, regex("sighting|observed")) | 
        str_detect(Method_Cap, regex("sighting")) ~ "sighting only",
      is.na(Method_Cap) ~ "unknown",
      .default = Method_Cap),
    Fate = case_when(PM_Notes == "saw donated" ~ "killed", .default = Fate),
    Saw_Kept = case_when(Saw_Kept == 'yes' ~ 'y',
                         Saw_Kept == 'no' ~ 'n',
                         .default = Saw_Kept),
    Sex = case_when(Num_Pups > 0 ~ "f", 
                    .default = Sex) 
  ) #Finally!!!

## fix species names----
timeline <- aushist %>% 
  group_by(Basin) %>%
  dplyr::select(-ID2) %>% 
  mutate(mean_lat = mean(Latitude, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Basin = fct_reorder(Basin, mean_lat),
         Region = fct_collapse(Region, "FITZROY" = c("FITZROY", "CURTIS")),
         Region = fct_reorder(Region, mean_lat),
         Species = fct_collapse(Species,
                                "A. cuspidata" = c("A. cuspidata", "Anoxypristis cuspidata"),
                                "P. clavata" = c("P. clavata", "Pristis clavata"),
                                "P. pristis" = c("P. pristis", "Pristis pristis"),
                                "P. clavata / P. pristis" = "P. pristis or P. clavata",
                                "P. zijsron" = c("P. zijsron", "Pristis zijsron"),
                                "A. cuspidata / P. zijsron" = "P. zijsron or A. cuspidata",
                                "Pristis sp." = c("Pristis sp.", "Pristis spp.")),
         Spec_Conf = case_when(Spec_Acc >=1 ~ 1,
                               Species == "Pristiophorus sp." | Species == "Unknown" ~ 3,
                               .default = 2), # labelled sawfish, but little cap details; unconfirmed
         Data_Met = fct_collapse(Medium,
                                 "News/Media" = c("News", "Journal/Magazine", "Book"),
                                 "SARA Database" = c("Submission", "Private collection", "Sale"),
                                 "Public Institution" = c("Museum Record", "Display"))) %>% 
  mutate(Species = factor(Species, levels = c(
    "P. clavata", "P. pristis", "P. clavata / P. pristis", "A. cuspidata", 
    "P. zijsron", "A. cuspidata / P. zijsron", "Pristis sp.", "Pristidae", "Pristiophorus sp."
  )))

# save it----
write_xlsx(timeline, "data/xls/processed/timeline.xlsx") #all historical data but with cleaned up names - QLD/NSW only

