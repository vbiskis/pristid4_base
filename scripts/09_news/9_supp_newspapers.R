# Details ----
#' 9_supp_newspaper.R
#' Paper: [Est. extent of range contraction and extinction timeline]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 2025-6-14
#' Content: 
#' make supps figures of publication range
#' -----------

#needs edits! for auto fxn
#but works fine
source('helpers/help_news.R')
library(MetBrewer)

allhist <- readxl::read_xlsx("data/xls/processed/allhist.xlsx") 
EC_Spec <- readxl::read_xlsx("data/xls/processed/EC_Spec.xlsx") 
trove <- readxl::read_xlsx("data/xls/News_SU.xlsx") #news
trove_all <- readxl::read_xlsx("data/xls/TroveDat_ALLArts.xlsx") 

region_lats <- allhist %>% 
  dplyr::select(Latitude, Region) %>% 
  filter(!is.na(Latitude), !is.na(Region)) %>%  # remove NAs
  group_by(Region) %>% 
  summarise(mean_lat = mean(Latitude, na.rm = TRUE), .groups = 'drop') %>%
  distinct() #for ordering graph

backup_lats <- EC_Spec %>%
  dplyr::select(Latitude, Region) %>% 
  filter(!is.na(Latitude), !is.na(Region)) %>% 
  group_by(Region) %>% 
  summarise(mean_lat = mean(Latitude, na.rm = TRUE), .groups = 'drop') %>%
  distinct()

#just quick on whether it was really a sawfish too
trove_real <- trove %>% 
  filter(Species != "Unknown", Species != "Pristiophorus sp.")

trove_conf <- trove_real %>% 
  filter(Spec_Acc > 0)

trove_all_sfo <- trove_all %>% 
  filter(SFN %in% trove_real$SFN) %>% 
  mutate(trove_conf = case_when(
    SFN %in% trove_conf$SFN ~ "Verified",
    TRUE ~ "Unverified"
  ))

np_TL <- trove_all_sfo %>% 
  group_by(State_NP, NP_Region, NP_Basin, NP_Loc, #for facet
           NP_RegionCap, NP_BasinCap, NP_Rural, #for adding symbols
           NP_Name_Fin) %>% #det how many in an area
  summarise(
    first_record = min(Year_Art),
    last_record = max(Year_Art),
    total_span = max(Year_Art) - min(Year_Art) + 1,
    
    total_records = n(),
    unique_years = n_distinct(Year_Art)) %>% 
  left_join(region_lats, by = c("NP_Region" = "Region")) %>% 
  left_join(backup_lats, by = c("NP_Region" = "Region"), suffix = c("", "_backup")) %>% 
  mutate(mean_lat = coalesce(mean_lat, mean_lat_backup)) %>%
  dplyr::select(-mean_lat_backup)

#lets go individually----
np_qld <- np_TL %>% 
  filter(State_NP == "QLD" | NP_Region == "GOLD COAST") %>% 
  mutate(NP_Region = case_when(NP_Region %in% c("WEIPA", "LAKE EYRIE") ~ "OUTSIDE",
                               .default = NP_Region))

region_lat_order <- np_qld %>%
  group_by(NP_Region) %>%
  mutate(mean_lat = case_when(NP_Region == "OUTSIDE" ~ NA, 
                              .default = mean_lat)) %>% 
  summarise(region_mean_lat = mean(mean_lat, na.rm = TRUE), .groups = 'drop') %>%
  arrange(desc(region_mean_lat)) %>%  # North to South
  pull(NP_Region)

# Then order everything properly
np_qld <- np_qld %>%
  mutate(NP_Region = factor(NP_Region, levels = region_lat_order)) %>%  
  arrange(NP_Region, desc(mean_lat), NP_Basin, first_record) %>%
  group_by(NP_Region) %>%
  mutate(y_pos = row_number()) %>%
  ungroup()

#show active papers in each region
ggplot(np_qld, aes(y = y_pos)) +
  geom_segment(aes(x = first_record, xend = last_record, color = NP_Basin), 
               size = 2, alpha = 0.7) +
  geom_point(aes(x = first_record, color = NP_Basin, shape = NP_Rural), size = 2) +
  geom_point(aes(x = last_record, color = NP_Basin, shape = NP_Rural), size = 2) +
  facet_grid(NP_Region ~ ., scales = "free_y", space = "free_y") +
  scale_y_continuous(expand = expansion(add = c(0.75, 0.75))) +
  scale_color_manual(values = met.brewer('Redon', 18)) +
  labs(x = "Year", y = expression("Publications (S " * symbol("\256") * " N)"), 
       title = "",
       color = "Basin") +
  theme_few() +
  theme(
    strip.text.y = element_text(angle = 0, 
                                face = "bold", family = "optima"),  
    strip.background = element_rect(fill = "grey95", color = "grey90"),
    axis.text.y = element_blank(), axis.text.x = element_text(family = "optima"),
    axis.title = element_text(family = "optima"),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "none"
  )

ggsave(
  "qld_papers.png",
  plot = last_plot(),
  device = NULL,
  path = "figs/s10/",
  scale = 1,
  width = 6,
  height = 8,
  units = c("in", "cm", "mm", "px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
)

##get counts----
qld_counts <- np_qld %>%
  rowwise() %>%
  do(data.frame(
    NP_Region = .$NP_Region,
    NP_Basin = .$NP_Basin,
    NP_Name = .$NP_Name,
    Year = seq(.$first_record, .$last_record)
  )) %>%
  ungroup() %>%
  group_by(NP_Region, Year) %>%
  summarise(
    concurrent_papers = n(),
    .groups = 'drop'
  )

# Find max concurrent papers per region
qld_conc <- qld_counts %>%
  group_by(NP_Region) %>%
  summarise(
    max_concurrent = max(concurrent_papers),
    peak_year = Year[which.max(concurrent_papers)],
    .groups = 'drop'
  )

#same with NSW----
np_nsw <- np_TL %>% 
  filter(State_NP == "NSW" & NP_Region != "GOLD COAST") %>% 
  mutate(NP_Region = case_when(NP_Region %in% c("MURRAY-DARLING", "SNOWY-SHOALHAVEN") ~ "OUTSIDE",
                               .default = NP_Region))

lat_order_nsw <- np_nsw %>%
  group_by(NP_Region) %>%
  mutate(mean_lat = case_when(NP_Region == "OUTSIDE" ~ NA, 
                              .default = mean_lat)) %>%  #manually fix orders
  summarise(region_mean_lat = mean(mean_lat, na.rm = TRUE), .groups = 'drop') %>%
  arrange(desc(region_mean_lat)) %>%  # North to South
  pull(NP_Region)

np_nsw <- np_nsw %>%
  mutate(NP_Region = factor(NP_Region, levels = lat_order_nsw)) %>%  
  arrange(NP_Region, desc(mean_lat), NP_Basin, first_record) %>%
  group_by(NP_Region) %>%
  mutate(y_pos = row_number()) %>%
  ungroup()

ggplot(np_nsw, aes(y = y_pos)) +  
  geom_segment(aes(x = first_record, xend = last_record, color = NP_Basin), 
               size = 2, alpha = 0.7) +
  geom_point(aes(x = first_record, color = NP_Basin, shape = NP_Rural), size = 2) +
  geom_point(aes(x = last_record, color = NP_Basin, shape = NP_Rural), size = 2) +
  facet_grid(NP_Region ~ ., scales = "free_y", space = "free_y") +
  scale_y_continuous(expand = expansion(add = c(1.25, 1.25))) +
  scale_color_manual(values = met.brewer('Navajo', 23)) +
  labs(x = "Year", y = expression("Publications (S " * symbol("\256") * " N)"), 
       title = "",
       color = "Basin") +
  theme_few() +
  theme(
    strip.text.y = element_text(angle = 0, 
                                face = "bold", family = "optima"),  
    strip.background = element_rect(fill = "grey95", color = "grey90"),
    axis.text.y = element_blank(), axis.text.x = element_text(family = "optima"),
    axis.title = element_text(family = "optima"),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "none"
  )

ggsave(
  "nsw_papers.png",
  plot = last_plot(),
  device = NULL,
  path = "figs/s10/",
  scale = 1,
  width = 6,
  height = 8,
  units = c("in", "cm", "mm", "px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
)

nsw_counts <- np_nsw %>%
  rowwise() %>%
  do(data.frame(
    NP_Region = .$NP_Region,
    NP_Basin = .$NP_Basin,
    NP_Name = .$NP_Name,
    Year = seq(.$first_record, .$last_record)
  )) %>%
  ungroup() %>%
  group_by(NP_Region, Year) %>%
  summarise(
    concurrent_papers = n(),
    .groups = 'drop'
  )

# Find max concurrent papers per region
nsw_conc <- nsw_counts %>%
  group_by(NP_Region) %>%
  summarise(
    min_concurrent = min(concurrent_papers),
    max_concurrent = max(concurrent_papers),
    peak_year = Year[which.max(concurrent_papers)],
    .groups = 'drop'
  )

#also how many towns in each basin? how many newspapers in each basin?----
np_qld %>%
  group_by(NP_Region) %>% 
  summarise(
    n_basins = n_distinct(NP_Basin),
    n_towns = n_distinct(NP_Loc),
    news = n_distinct(NP_Name_Fin), #number of papers
    npt = news/n_towns #newspapers/town
  )

np_nsw %>%
  group_by(NP_Region) %>% 
  summarise(
    n_basins = n_distinct(NP_Basin), #number of basins in this region with a newspaper
    n_towns = n_distinct(NP_Loc), #number of towns in the region with a 
    news = n_distinct(NP_Name_Fin), #number of papers
    npt = news/n_towns #newspapers/town
  )
