# Details ----
#' 2_appendix_news_table.R
#' Paper: [Est. extent of range contraction and extinction timeline]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 2025-11-20
#' Content: for supps: table of all news sources
#' Output: 
#' + appendix table s1 (app_s1.docx)
#' + appendix table (abbridged) - newspaper summary (app_s1b.docx)

#' -----------

pacman::p_load("ggplot2", 
               "dplyr",
               "readxl",
               "flextable")

troveall <- readxl::read_xlsx("data/xls/TroveDat_ALLArts.xlsx") #for newspaper citation

# trove table----
unique(troveall$ArtType) #k what to filter out

## shortened version, just ID no links----
troveall %>% 
  filter(!ArtType %in% c('MisID - Sawshark',
                         'MisID - Other'),
         !is.na(Latitude), !is.na(Year_Ev)) %>% 
  group_by(State_NP, NP_Name_Fin) %>% 
  summarise(
    pub = first(NP_Loc),
    num_saws = n_distinct(SFN[ArtType %in% c('Museum/Private Collections',
                                             'Sawfish capture/sighting', 
                                             'Research/Expedition')]),
    num_arts = n(),
    main = sum(Main, na.rm = TRUE),
    pubtype = paste0(num_saws, " (", main, ", ", num_arts - num_saws, ")"), #have extra pubs there in brackets
    TVNs = paste(unique(TVN), collapse = ", ")
  ) %>% 
  select(State_NP, pub, NP_Name_Fin, pubtype, TVNs) %>% 
  flextable() %>%
  save_as_docx(path = "figs/s1/newspapers_summ.docx")

## full final, in citation format for all articles----
troveall %>% 
  filter(!ArtType %in% c('MisID - Sawshark',
                         'MisID - Other'),
         !is.na(Latitude), !is.na(Year_Ev)) %>% 
  group_by(SFN, TVN) %>% 
  summarise(
    cap = paste0(Cap_Loc, ", ", State_Cap),
    title = first(Art_Title, na_rm = TRUE),
    main = first(Main == 1),
    citstart = paste0(NP_Name, ". (", Year_Art, "). "),
    citend = paste0(", ", NP_Loc, ", ", State_NP, ". ", 
                    Day_Art, " ", month.abb[Mo_Art],
                    ", p. ",  NP_pg, ". ", "[", Art_link, "].")) %>% 
  arrange(SFN, desc(main)) %>%
  select(SFN, main, citstart, title, citend) %>%  
  flextable() %>% 
  compose(j = "citstart", 
          value = as_paragraph(citstart, as_i(title), citend)) %>%
  bold(i = ~ main == TRUE) %>%
  set_header_labels(citstart = "Citation") %>%
  delete_columns(j = c("main", "title", "citend")) %>%
  merge_v(j = "SFN") %>%
  width(j = "SFN", width = 2.75) %>%
  width(j = "citstart", width = 13.75) %>%
  save_as_docx(path = "figs/s1/newspapers_cits.docx")

##just main arts----
troveall %>% 
  filter(ArtType %in% c('Museum/Private Collections',
                        'Sawfish capture/sighting', 
                        'Research/Expedition'),
         !is.na(Latitude), !is.na(Year_Ev),
         Main == 1) %>% 
  group_by(SFN, TVN) %>% 
  summarise(
    cap = paste0(Cap_Loc, ", ", State_Cap),
    title = first(Art_Title, na_rm = TRUE),
    year = first(Year_Art, na_rm = TRUE),
    citstart = paste0(NP_Name, ". (", Year_Art, "). "),
    citend = paste0(", ", NP_Loc, ", ", State_NP, ". ", 
                    Day_Art, " ", month.abb[Mo_Art],
                    ", p. ",  NP_pg, ". ", "[", Art_link, "].")) %>% 
  arrange(SFN, desc(year)) %>%
  select(SFN, citstart, title, citend) %>%  
  flextable() %>% 
  compose(j = "citstart", 
          value = as_paragraph(citstart, as_i(title), citend)) %>%
  set_header_labels(citstart = "Citation") %>%
  delete_columns(j = c("title", "citend")) %>%
  merge_v(j = "SFN") %>%
  width(j = "SFN", width = 2.75) %>%
  width(j = "citstart", width = 13.75) %>%
  save_as_docx(path = "figs/s1/news_mains.docx")
