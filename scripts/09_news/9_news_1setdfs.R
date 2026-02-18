# Details ----
#' help_news.R
#' Paper: [Est. extent of range contraction and extinction timeline]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 2025-12-1
#' Content: making a src code for prep of our dfs!
#' -----------

library(stringdist)
source('helpers/help_news.R')

# load dfs----
trove_all <- readxl::read_xlsx("data/xls/TroveDat_ALLArts.xlsx") 
filthist <- readxl::read_xlsx("data/xls/processed/filthist.xlsx") 

#get sawsharks----
pristio <- filthist %>% 
  filter(Species %in% c('Pristiophorus sp.', 'Unknown') |
           ID %in% c('TRV0010', 'TRV0191'))

trove_main <- trove_all %>% 
  left_join(filthist %>% 
              dplyr::select(ID, Basin, Region), #get the basins and region
            by = c("SFN" = "ID")) %>% 
  filter(!SFN %in% pristio$ID,) %>%  #awesome none of those silly guys
  dplyr::select(TVN, SFN, Main, n_Art, NP_pg,
                Day_Ev, Mo_Ev, Year_Ev, 
                Day_Art, Mo_Art, Year_Art,
                State_NP, NP_Loc, NP_Name_Fin, NP_Rural,
                PicArt, ArtType, 
                NP_BasinCap, NP_Region, Basin, Region, # Zone, later
                Species, TL_final, `Full text`, Art_Title) %>% 
  mutate(date_a = make_date(Year_Art, Mo_Art, Day_Art), #fix the date col! ugh! dates!
         date_ev = make_date(Year_Ev, Mo_Ev, Day_Ev),
         Period = case_when(Year_Art <= 1900 ~ "Pre-1900", #pre-Federation
                            Year_Art > 1900 & Year_Art <= 1915 ~ "1901-1915", #pre WWI
                            Year_Art > 1915 & Year_Art <= 1930 ~ "1916-1930", #post WWI
                            Year_Art > 1930 & Year_Art <= 1945 ~ "1931-1945", #pre WWII
                            Year_Art > 1945 & Year_Art <= 1960 ~ "1946-1960", #post WWII
                            Year_Art > 1960 & Year_Art <= 1975 ~ "1961-1975", #modern fishing
                            Year_Art > 1975 ~ "1976", #pre-conservation
                            .default = NA)) %>% 
  filter(ArtType %in% c('Museum/Private Collection', 'Sawfish capture/sighting'))%>% #just real events
  mutate(Period = factor(Period, levels = c("Pre-1900", "1901-1915", "1916-1930",
                                            "1931-1945", "1946-1960", "1961-1975")))

trove_main <- trove_main %>% 
  group_by(SFN) %>% 
  mutate(TL_final = ifelse(is.na(TL_final), 
                           mean(TL_final, na.rm = TRUE), 
                           TL_final),
         TL_final = ifelse(is.nan(TL_final), NA, TL_final)
  ) %>% 
  ungroup()

write_xlsx(trove_main, 'data/xls/processed/trove_main.xlsx')

# filter for duplicates----

#some of the records are just exact duplicates, published as a syndication
#lets get rid of those, and just tally them on the side.

duplicates <- trove_main %>%
  group_by(SFN, Art_Title) %>% #for any article title in a unique sawfish event
  group_modify(~{
    if(nrow(.x) > 1) {
      sim_matrix <- 1 - stringdistmatrix(.x$`Full text`, method = "jaccard", q = 3)
      .x$dup_group <- cutree(hclust(as.dist(1 - sim_matrix)), h = 0.1)
    } else {
      .x$dup_group <- 1
    }
    .x
  })

# great! now simplify
trove_m_arts <- duplicates %>% 
  group_by(SFN, Art_Title, dup_group) %>%
  slice_min(date_a, n = 1, with_ties = FALSE) %>% #starting with the earliest one
  ungroup() # lose > 100 that are just syndicated entirely

write_xlsx(trove_m_arts, 'data/xls/processed/trove_m_arts.xlsx')
