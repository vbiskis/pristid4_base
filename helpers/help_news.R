# Details ----
#' help_news.R
#' Paper: [Est. extent of range contraction and extinction timeline]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 2025-12-1
#' Content: making a src code for prep of our dfs!
#' -----------

pacman::p_load('dplyr', 'tidyverse', 'readxl', 'writexl', 'lubridate', #data cleaning
               'wordcloud', 'tidytext', 'tm', #words
               'ggplot2', 'ggpubr', 'pilot', 'MetBrewer') #and plot themes 

suppressWarnings({mytheme <- pilot::theme_pilot(axis_title_size = 12, legend_title_size = 12,
                                                legend_title_family = "Optima", legend_text_family = "Optima",
                                                facet_title_family = "Optima", facet_title_size = 12,
                                                subtitle_family = "optima", 
                                                axis_title_family = "Optima", axis_text_family = "Optima") 
}) # my god shes loud
