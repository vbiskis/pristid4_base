# Details ----
#' 1_dfs1_set.R
#' Paper: [Est. extent of range contraction and extinction timeline]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 2025-05-07
#' Content: combines dfs from all sources
#'  + and pulls missing data between them for duplicates
#' Output: allhist.xlsx + filthist.xlsx (dups removed)
#' - still needs extra cleaning/processing for analysis
#' -----------

pacman::p_load('readxl', 'writexl', 
               'tidyverse', 'dplyr', 'tidyr')

# Bring in----
trove <- readxl::read_xlsx("data/xls/News_SU.xlsx") #news
m_saws <- readxl::read_xlsx("data/xls/Museum_Saws.xlsx") #saws
oldsubs <- readxl::read_xlsx("data/xls/Hist_Subs.xlsx") #sightings
s_saws <- readxl::read_xlsx("data/xls/SARA_Saws.xlsx") #saws

# Wrangle sheets----
# news----
# you first bc you're gospel
news1 <- trove %>% #only basic details to get time/location info
  dplyr::filter(!ArtType %in% c("Information - biology", "Information - fishing value")) %>% 
  dplyr::select(SFN, Spec_Acc, Species, NP_Name, n_Art,
                SUB, Fisher, Fisher_Type, #to filter overlap
                River, Basin, Region, Division, #spatial info
                Latitude, Longitude, State_Cap, Loc_Acc,
                Day_Ev, Mo_Ev, Year_Ev, Date_Acc, #temporal info
                Ab, Fate, Method_Cap, Method_Death, Saw_Kept, PM_Notes, #sawfish info
                TL_final, Saw_cm, SRL, Age, LHS, Mat, Sex, Num_Pups) %>%
  mutate(Dataset = "trove",  Medium = "News", Mode = "No relation", Type = "Published media",
         Age = as.numeric(Age),
         Saw_cm = as.numeric(Saw_cm), 
         Abund = case_when(Ab == "> 1" ~ 2,
                           .default = as.numeric(Ab))) %>% 
  rename(ID = SFN, ID2 = SUB,
         Day = Day_Ev, Month = Mo_Ev, Year = Year_Ev,
         State = State_Cap, Medium_Name = NP_Name,
         TL = TL_final) %>% 
  dplyr::select(-Ab)

news1 <- news1 %>%
  mutate(across(c(Day, Month, Year, Age), as.numeric))

# museums----
mus1 <- m_saws %>% #only basic details to get time/location info
  filter(Country == 'Australia' | Country == 'PNG',
         !Type %in% c('Muscle', 'DNA Sample')) %>% 
  mutate(Saw_cm = as.numeric(TTC)/10) %>% 
  dplyr::select(ID, Spec_Acc, Species,
                ID2, Museum, Donor, Type, #to filter overlap
                River, Basin, Region, Division, #spatial info
                Lat, Long, State, Loc_Acc,
                Day, Month, Year, Date_Acc, #temporal info
                Method_Cap, #sawfish info
                TL, Saw_cm, SRL, Age, LHS, Sex, Mat) %>% 
  mutate(Spec_Acc = as.numeric(Spec_Acc), #idk why he loads like that
         Age = as.numeric(Age),
         Dataset = "museums", Medium = "Museum Record", Mode = "Donated",
         SRL = SRL/10, #in mm
         Type = case_when(Type %in% c("Photo", "Picture") ~ "Photograph",
                          .default = "Saw"),
         Saw_Kept = if_else(Type == "Saw", "y", NA),
         PM_Notes = "saw donated",
         Num_Pups = NA, Abund = 1) %>% 
  rename(Latitude = Lat, Longitude = Long, Medium_Name = Museum, Fisher = Donor) %>% 
  dplyr::filter(is.na(Year) | Year <= 2000) 
#this one's funny - there are some pups and mum, so multiples of an event
#let's quickly fix this

mus2 <- mus1 %>%
  mutate(
    # first, identify which rows are duplicates (ID2 matches another ID)
    is_extra = ID2 %in% ID & !is.na(ID2), 
  ) %>%
  mutate(
    # we need to know what kind of addl specimen it is!
    abu_count = sapply(ID, function(x) sum(ID2 == x & is.na(LHS), na.rm = TRUE)), #no life history info
    pup_count = sapply(ID, function(x) sum(ID2 == x & LHS %in% c("neonate", "YOY"), na.rm = TRUE)) #or its the aborted pup
  ) %>%
  mutate(
    Abund = if_else(!is_extra, Abund + abu_count, Abund),
    Num_Pups = if_else(!is_extra, pup_count, NA) %>% 
      na_if(0)) %>%
  filter(!is_extra) %>% #and get rid of those extra rows
  dplyr::select(-is_extra, -abu_count, -pup_count) #and the temp counting cols

# subs----
sub1 <- oldsubs %>% #only basic details to get time/location info
  dplyr::select(SUB, Species, Spec_Acc,
                OtherID, Fisher_Name, #to filter overlap
                River, Basin, Region, Division, #spatial info
                LAT, LONG, State, Loc_Acc,
                Capture_Day, Capture_Month, Capture_Year, Date_Acc, #temporal info
                Medium, Mode, Type, Medium_Name, #record info
                Ab, Fate, Method_Cap, Method_Death, PM_Notes, Saw_Kept, #sawfish info
                TL, Saw_cm, SRL, Age, LHS, Mat, Sex, Num_Pups) %>% 
  mutate(Dataset = "cytags",
         Age = as.numeric(Age),
         Abund = case_when(Ab == "> 1" ~ 2, Ab == ">1" ~ 2,
                           Ab == ">2" ~ 3, Ab == "Multiple" ~ 3,
                           .default = as.numeric(Ab))) %>%
  rename(ID = SUB, ID2 = OtherID, Fisher = Fisher_Name,
         Latitude = LAT, Longitude = LONG, 
         Day = Capture_Day, Month = Capture_Month, Year = Capture_Year) %>% 
  dplyr::select(-Ab)

# saws----
saw1 <- s_saws %>% #only basic details to get time/location info
  mutate(Saw_cm = as.numeric(TTC)/10) %>% 
  dplyr::select(ID, Species, 
                ID2, Fisher, #to filter overlap
                River, Basin, Region, Division, #spatial info
                Latitude, Longitude, State, Loc_Acc,
                Year, Date_Acc, #temporal info
                Medium, Mode, Medium_Name, #record info
                Method_Cap, #sawfish info
                TL, Saw_cm, SRL, Age, LHS, Mat) %>%
  mutate(Spec_Acc = 1,
         Dataset = "saws",
         SRL = SRL/10, #in mm
         Type = "Saw",
         Saw_Kept = "y",
         PM_Notes = "saw donated",
         Abund = 1) 

# Patch Vals----
# Just to the main sheet(s) really
sub2 <- sub1 %>%
  filter(!(ID2 %in% ID)) %>% #just in case I have marked a duplicate event in subs
  rows_patch(saw1 %>% dplyr::
               select(ID2 = ID, 
                      Fisher, TL, SRL, Saw_cm, 
                      Age, LHS, Mat), 
             by = "ID2", 
             unmatched = "ignore")

news1 <- news1 %>%
  rows_patch(mus2 %>%
               filter(!is.na(ID2)) %>%  # Remove NA values
               dplyr::select(ID = ID2, Fisher,
                      TL, SRL, Saw_cm, 
                      Age, LHS, Mat, Sex, Num_Pups), 
             by = "ID", 
             unmatched = "ignore") %>% 
  rows_patch(sub2 %>%
               filter(!is.na(ID2)) %>%  # Remove NA values
              dplyr::select(ID = ID2, Fisher,
                      TL, SRL, Saw_cm, 
                      Age, LHS, Mat, Sex, Num_Pups), 
             by = "ID", 
             unmatched = "ignore") %>%  
  rows_patch(saw1 %>% filter(!is.na(ID2)) %>%  # Remove NA values
              dplyr::select(ID = ID2, Fisher,
                      TL, SRL, Saw_cm, 
                      Age, LHS, Mat), 
             by = "ID", 
             unmatched = "ignore") #got him working!

# Remove Dups----
mus3 <- mus2 %>%
  anti_join(news1, by = c("ID2" = "ID"))

sub3 <- sub2 %>%
  anti_join(news1, by = c("ID2" = "ID")) %>%
  anti_join(mus1, by = c("ID2" = "ID"))

saw2 <- saw1 %>%
  anti_join(news1, by = c("ID2" = "ID")) %>%
  anti_join(mus1, by = c("ID2" = "ID")) %>%
  anti_join(sub1, by = c("ID2" = "ID"))

# And Bind----
allhist <- bind_rows(news1, mus1, sub1, saw1)
allhist <- allhist %>% 
  filter(Year <= 1990 | is.na(Year))
filthist <- bind_rows(news1, mus3, sub3, saw2)
filthist <- filthist %>% 
  filter(Year <= 1990 | is.na(Year))

write_xlsx(allhist, 'data/xls/processed/allhist.xlsx') #all data, including duplicates
write_xlsx(filthist, 'data/xls/processed/filthist.xlsx') #no duplicates

