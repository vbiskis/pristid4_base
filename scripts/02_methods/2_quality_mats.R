# Details ----
#' 2_quality_control.R
#' Paper: [Est. extent of range contraction and extinction timeline]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 2025-11-27
#' Content: look at dfs pre/post filtering
#' Input: 
#' + allhist.xlsx (no filters)
#' + filthist.xlsx (duplicates removed)
#' + EC_Date.xlsx (final cleaned df)
#' Output: 
#' + heatmaps of data quality
#' -----------

source('helpers/help_stat.R')

allhist <- readxl::read_xlsx("data/xls/processed/allhist.xlsx") 
filthist <- readxl::read_xlsx("data/xls/processed/filthist.xlsx") 
EC_Date <- readxl::read_xlsx("data/xls/processed/EC_Date.xlsx") 

qualctrl <- filthist %>% 
  filter(is.na(Year) | Year <= 1990) %>% 
  mutate(Spec_Acc = case_when(Spec_Acc == 0  ~ 4,
                              .default = Spec_Acc),
         Date_Acc = case_when(Date_Acc == 0  ~ 5,
                              .default = Date_Acc),
         Type_S = fct_collapse(Type,
                               "Photograph" = c("Photo", "Photo/Cast", "Photo/Saw"),
                               "Specimen" = c("Saw", "Whole specimen", "Saw, Indigenous artwork")),
         Rec_Acc = case_when(Date_Acc <= 2 & Spec_Acc <= 2 & Loc_Acc <= 2 &
                               (Type_S %in% c("Photograph", "Published media") |  
                                  Medium %in% c("Museum Record", "Display")) ~ 1,
                             (Date_Acc <= 3 & Spec_Acc <= 3 & Loc_Acc <= 3) & 
                               (Type_S %in% c("Photograph", "Published media") | 
                                  Medium %in% c("Museum Record", "Display", "Private collection")) ~ 2,
                             (Date_Acc <= 3 & Spec_Acc <= 3 & Loc_Acc <= 3) &
                               (Type_S %in% c("Encounter", "Specimen") |  
                                  Medium == "Private Collection") ~ 3,
                             .default = 0))

# quality matrices----
## test on goc----
goc <- qualctrl %>% 
  filter(Division == "GULF OF CARPENTARIA") #134 total

goc_summary <- goc %>% 
  group_by(Basin, Dataset) %>% 
  summarise(
    n = n(),
    q1 = sum(Rec_Acc == 1),
    q2 = sum(Rec_Acc == 2),
    q3 = sum(Rec_Acc == 3),
    q4 = sum(Rec_Acc == 0),
    .groups = "drop"
  ) 

goc_summary %>%
  bind_rows(
    summarise(.,
              Basin = "TOTAL",
              Dataset = "",
              n = sum(n),
              q1 = sum(q1),
              q2 = sum(q2),
              q3 = sum(q3),
              q4 = sum(q4))
  ) %>%
  print(n = Inf)

goc_qual <- goc %>% 
  filter(Rec_Acc < 3 & Rec_Acc > 0)

sum(!is.na(goc_qual$TL)) #60 of 64

goc_qual %>% 
  group_by(Medium, Type_S) %>% 
  summarise(
    n = n()
  )

#visualise----
goc_counts <- goc_qual %>%
  group_by(Type_S, Spec_Acc, Loc_Acc, Date_Acc) %>%
  summarise(count = n(), .groups = "drop")

ggplot(goc_counts, 
       aes(x = factor(Date_Acc), y = factor(Loc_Acc))) +
  geom_tile(aes(fill = count)) +
  geom_text(aes(label = count), color = "white") +
  facet_wrap(Type_S ~ Spec_Acc, labeller = label_both) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  theme_minimal() +
  labs(
    title = "Data Quality Matrix: Location × Date (faceted by Species ID quality)",
    x = "Date Quality (1=high, 5=low)",
    y = "Location Quality (1=high, 4=low)",
    fill = "Count"
  ) #low date accuracy in general


