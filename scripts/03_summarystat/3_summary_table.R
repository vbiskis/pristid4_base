# Details ----
#' 3_summary_table.R
#' Paper: [Est. extent of range contraction and extinction timeline]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 2025-06-07
#' Content: look at all dfs pre/post filtering
#' Input: 
#' + allhist.xlsx (no filters)
#' + filthist.xlsx (duplicates removed)
#' + EC_Date.xlsx (final cleaned df)
#' Output: Table 5.2
#' -----------

pacman::p_load('readxl', 'dplyr', 'tidyverse')

allhist <- readxl::read_xlsx("data/xls/processed/allhist.xlsx") 
filthist <- readxl::read_xlsx("data/xls/processed/filthist.xlsx") 
EC_Date <- readxl::read_xlsx("data/xls/processed/EC_Date.xlsx") 

# make it all one table----
# this bit was very repetitive
## time for a fxn!

stdz_cats <- function(df) {
  df %>%
    mutate(
      Source = case_when(
        Dataset == "trove" ~ "NLA",
        Dataset == "museums" | Medium %in% c("Museum Record", "Display") ~ "Institution", 
        Dataset %in% c("cytags", "saws") ~ "SARA"
      ),
      Type_S = case_when(
        Type %in% c("Photo", "Photo/Cast", "Photo/Saw") ~ "Photograph",
        Type == "Encounter" ~ "Account",
        Type == "Published media" ~ "News",
        Type %in% c("Saw", "Saw, Indigenous artwork", "Whole specimen") ~ "Specimen",
        TRUE ~ Type
      )
    )
}

# Table 2----

## Get all parts----
init_count <- allhist %>% # unfiltered
  stdz_cats() %>%
  group_by(Type_S, Source) %>%
  summarise(Initial = n(), .groups = "drop")

data_flow <- filthist %>% # duplicates removed
  stdz_cats() %>%
  group_by(Type_S, Source) %>%
  summarise(
    AfterD = n(),
    On_EC = sum(ID %in% EC_Date$ID, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(init_count, by = c("Type_S", "Source")) %>%
  dplyr::select(Type_S, Source, Initial, AfterD, On_EC)

conf_counts <- EC_Date %>% # what made it to the final df
  stdz_cats() %>%
  mutate(
    Confirmation = case_when(
      Rec_Acc == 1 ~ "V1",
      Rec_Acc == 2 ~ "V2",
      Rec_Acc == 3 ~ "P",
      Rec_Acc == 0 ~ "U"
    )
  ) %>%
  group_by(Type_S, Source, Confirmation) %>% # row, col a, col b
  summarise(n = n(), .groups = "drop") %>%
  pivot_wider(names_from = Confirmation, values_from = n, values_fill = 0) %>%
  mutate(V = V1 + V2) # levels considered valid 

## Combine---- 
### Data inside Table 2----
final_table <- data_flow %>%
  left_join(conf_counts, by = c("Type_S", "Source")) %>%
  arrange(Type_S, Source) %>%
  rename(Type = Type_S) %>%
  dplyr::select(Type, Source, Initial, AfterD, On_EC, V, V1, V2, P, U)

### Totals----
final_table %>% # sources total
  group_by(Source) %>% 
  summarise(
    across(3, \(x) sum(x, na.rm = TRUE))
  )

final_table %>% # type total
  group_by(Type) %>% 
  summarise(
    across(3:9, \(x) sum(x, na.rm = TRUE))
  )

final_table %>% # all total
  summarise(
    across(4:9, \(x) sum(x, na.rm = TRUE))
  )

#who got filtered out----
checkID <- setdiff(filthist$ID, EC_Date$ID)
check <- subset(filthist, ID %in% checkID)

check %>% 
  # filter(Division != "GULF OF CARPENTARIA" | 
  #          is.na(Division)) %>% 
  stdz_cats() %>%
  group_by(Type_S, Division, Date_Acc, Loc_Acc) %>% 
  summarise(
    n()
  ) %>% 
  print(n = Inf)

## manual check through option----
allhist %>% 
  filter(Dataset == "cytags",
         Medium %in% c('Book', 'Journal/Magazine', "News")) %>% 
  group_by(ID) %>% 
  select(Medium, Year, Basin, Medium_Name) %>% 
  print(n = Inf)

filthist %>%
  stdz_cats() %>%
  filter(Type_S == "Photograph") %>% 
  group_by(ID) %>% 
  select(Medium, Year, Basin, Medium_Name) %>% 
  print(n = Inf)
