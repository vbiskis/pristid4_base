# Details ----
#' help_cite.R
#' Paper: [Est. extent of range contraction and extinction timeline]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 2025-11-28
#' Content: a template on how to cite packages
#' Input: NA
#' Output: citations!
#' -----------

library(NCmisc)
library(knitr)
library(bib2df)
library(synthesisr)

source('helpers/help_map.R')
source('helpers/help_stat.R')

# test----
citation("terra")

#data wrangling----
list.functions.in.file("scripts/01_set/1_dfs1_set.R", alphabetic = TRUE)
list.functions.in.file("scripts/01_set/1_dfs3_filter.R", alphabetic = TRUE)
list.functions.in.file("scripts/03_summarystat/3_summary_table.R", alphabetic = TRUE)

#distribution
list.functions.in.file("scripts/01_set/1_maps_set.R", alphabetic = TRUE)
list.functions.in.file("scripts/03_summarystat/3_gg_density.R", alphabetic = TRUE)
list.functions.in.file("scripts/04_basinmaps/4_spec_mapping.R", alphabetic = TRUE)

#composition
list.functions.in.file("scripts/04_basinmaps/4_spec_comp_tests.R", alphabetic = TRUE)
list.functions.in.file("scripts/04_basinmaps/4_spec_similarity.R", alphabetic = TRUE)

#abundance/Lmax
list.functions.in.file("scripts/05_decline/5_gam_catch.R", alphabetic = TRUE)
list.functions.in.file("scripts/05_decline/5_stats_decline.R", alphabetic = TRUE)

#extinctions
list.functions.in.file("scripts/07_timelines/7_timeline_ext_fxn.R", alphabetic = TRUE)

#text analysis
list.functions.in.file("scripts/08_news/8_news_words.R", alphabetic = TRUE)

#fisher trends
list.functions.in.file("scripts/08_news/8_news_bias.R", alphabetic = TRUE)
list.functions.in.file("scripts/09_behaviour/9_fisher_trends.R", alphabetic = TRUE)

# First create the .bib file
packages <- c("base", "dplyr", "ggpubr", #wrangling/tables
              "purrr", "furrr", "readr", "writexl", "readxl",
              "ggplot2", "cowplot", "ggthemes", "patchwork", #plots
              "stats", "tidyr", "car", #general stats (anovas, etc.)
              "vegan", "indicspecies", "betapart", #spec comp
              "pheatmap", "corrplot", #corr visualisation
              "gratia", "marginaleffects", "mgcv", "performance", "DHARMa",
              "terra", "sf", "rnaturalearth", "ggspatial", #spatial mapping
              "ggridges", #density
              "stringr", "tm", "wordcloud", "tidytext") #that's all of em!

knitr::write_bib(packages, file = "results/bib/packages_ch4.bib")

# Convert to data frame
bib_df <- read_refs("results/bib/packages_ch4.bib")

# Export to RIS for EndNote
synthesisr::write_refs(bib_df, format = "ris", file = "results/bib/packages_ch4.ris")

ris_content <- readLines("results/bib/packages_ch4.ris")
ti_lines <- grep("^TI  -", ris_content)

for(i in rev(ti_lines)) {  # reverse order so indices don't shift
  ris_content <- append(ris_content, "TY  - COMP", after = i-1)
}

writeLines(ris_content, "results/bib/packages_ch4.ris")
