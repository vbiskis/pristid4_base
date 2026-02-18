# Details ----
#' 7_timeline_ext_results.R
#' Paper: [Est. extent of range contraction and extinction timeline]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 2025-6-14
#' Content: get extinction values (xls)
#' + Figure 9!
#' -----------

#timeline calcs
source('helpers/help_stat.R')
library(ggthemes)
library(writexl)

source("scripts/07_timelines/7_timeline_ext_fxn.R")
EC_Date <- readxl::read_xlsx("data/xls/processed/EC_Date.xlsx") # using unconfirmed for some methods
EC_Dens <- readxl::read_xlsx("data/xls/processed/EC_Dens.xlsx") # only reliable

#big question - is there a diff in record type?

EC_Date %>% 
  group_by(Zone) %>% 
  summarise(
    maxsize = max(TL, na.rm = TRUE),
    t_n = max(Year, na.rm = TRUE),
    t_n_less_1 = sort(Year, decreasing = TRUE)[2],
    t_n_less_2 = sort(Year, decreasing = TRUE)[3],
    t_n_less_3 = sort(Year, decreasing = TRUE)[4],
    n_sighting = n(),
    years = n_distinct(Year)
  )

EC_Date %>% 
  group_by(Region) %>% 
  summarise(
    t_n = max(Year, na.rm = TRUE),
    t_n_less_1 = sort(Year, decreasing = TRUE)[2],
    t_n_less_2 = sort(Year, decreasing = TRUE)[3],
    t_n_less_3 = sort(Year, decreasing = TRUE)[4],
    n_sighting = n(),
    years = n_distinct(Year)
  )

#all species gone from Brisbane S
#PP/PC - basically all other than FNQ
#AC - known to at least Baffle, likely Mary; not ext
#PZ - Shoalwater Bay

# Calculations!! ----

## By region----
# FNQ obvs makes no sense - all species potentially possible, seen in last 20 years
# w/ NQ: use as logic/qual check

# CQ: large animals (filter TL or anoxy?)

# SEQ: 
# P. pristis
# P. zijsron

# NNSW: All species (pz)

# MNSW: All species (pz)

# All----
regions <- rev(c("Mid Coast NSW", "North Coast NSW", # everything
             "South East QLD", # pristis and zijrson
             "Wide Bay QLD", # pristis and zijrson
             "Central QLD", # mature individuals
             "North QLD", # mature individuals
             "Far North QLD" # mature individuals
))

results_list <- list()

for(i in seq_along(regions)) {
  results_list[[i]] <- get_ext_timelines(EC_Date, "Zone", regions[i],
                                         confidence_col = "Rec_Acc", timefrom = 1990)
}

summary_table <- ext_stbl(results_list, regions)
print(summary_table)
write_xlsx(summary_table, "results/s8/extinction_analysis1990.xlsx")
#for all regions: supp

# Look at species----
#get rid of the anoxy and freshies
#have some species overlap tho
PZ <- EC_Date %>% 
  filter((Spec_Known == "P. zijsron" | 
            (Spec_Known == "Pristidae" & (is.na(TL) | TL > 150))) # not anoxy (small TL)
         & Spec_Known != "P. clavata" & Spec_Known != "P. pristis" & 
         Species != "P. clavata / P. pristis"
         ) %>% 
  mutate(
    Basin = fct_reorder(Basin, mean_lat),
    Region = fct_reorder(Region, mean_lat),
    )

regionspx <- c("SHOALWATER BAY", "FITZROY", "BURNETT", "MARY", 
               "BRISBANE", "GOLD COAST", "NORTHERN RIVERS", 
               "COFFS COAST", "HUNTER", "SYDNEY")

basinspx <- PZ %>%
  filter(Region %in% regionspx) %>%  # or whatever your region names are
  pull(Basin) %>%
  unique() %>%
  as.character()

basinspx <- basinspx[!is.na(basinspx)]

#basins
results_list <- list()

for(i in seq_along(basinspx)) {
  results_list[[i]] <- get_ext_timelines(PZ, "Basin", basinspx[i],
                                         confidence_col = "Rec_Acc", timefrom = 1990)
}

ext_stbl(results_list, basinspx)

#regions
resultsz <- list()

for(i in seq_along(regionspx)) {
  resultsz[[i]] <- get_ext_timelines(PZ, "Region", regionspx[i],
                                         confidence_col = "Rec_Acc", timefrom = 1990)
}

ext_stbl(resultsz, regionspx)
plot_timeline(resultsz, regionspx, title = "a)",
         current_year = 1990)

summary_et_pz <- ext_stbl(resultsz, regionspx)
write_xlsx(summary_et_pz, "figs/fig9/extinction_analysis_pz.xlsx")

# pristis:----
PP <- EC_Date %>% 
  filter(Spec_Known == "P. pristis" | Species == "P. clavata / P. pristis",
         Zone != "Cape York QLD") %>% 
  mutate(Zone2 = case_when(Zone %in% c("South East QLD", "Wide Bay QLD") ~ "SEQ",
                           .default = "NEQ"))

# pplist <- c("Far North QLD", "North QLD",
#             "Wide Bay QLD", "South East QLD")

pplist <- c("NEQ", "SEQ")

resultsp <- list()

for(i in seq_along(pplist)) {
  resultsp[[i]] <- get_ext_timelines(PP, "Zone2", pplist[i],
                                         confidence_col = "Rec_Acc", timefrom = 1990)
}

ext_stbl(resultsp, pplist) #damn thats sad
plot_timeline(resultsp, pplist, title = "b)",
                       current_year = 1990)

summary_et_pp <- ext_stbl(resultsp, pplist)
write_xlsx(summary_et_pp, "figs/fig9/extinction_analysis_pp.xlsx")

#graph both:
all_sig_levels <- c("p < 0.001", "p < 0.01", "p < 0.05", "p < 0.1", "p ≥ 0.1", 
                    "Bayesian")
all_colors <- c("p < 0.001" = "darkgreen", "p < 0.01" = "#2ca02c", 
                "p < 0.05" = "gold", "p < 0.1" = "#1f77b4",
                "p ≥ 0.1" = "#7f7f7f", "Bayesian" = "#9467bd")

pz_tl <- plot_timeline(resultsz, regionspx, title = "a)", current_year = 1990) + 
  scale_color_manual(values = all_colors, limits = all_sig_levels, drop = FALSE) +
  theme(legend.position = "bottom")

pp_tl <- plot_timeline(resultsp, pplist, title = "b)", current_year = 1990) + 
  scale_color_manual(values = all_colors, limits = all_sig_levels, drop = FALSE) +
  theme(legend.position = "none")  # Remove legend from second plot

# Combine 
p1 <- pz_tl + theme(legend.position = "none")
p2 <- pp_tl + theme(legend.position = "none",
                    axis.title.y = element_blank())

library(cowplot)
# Get the legend from the first plot
legend <- get_legend(pz_tl + theme(legend.position = "right",
                                   legend.box = "vertical"))

# Combine
p1 + (p2 / legend + plot_layout(heights = c(2, 1))) + plot_layout(widths = c(1.5, 1))

#got em!
ggsave(
  "figs/fig9/fig9_ext_tl.tiff",
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 9,
  height = 6,
  units = c("in", "cm", "mm", "px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
)

# east coast clavata:----
PC <- EC_Types %>% 
  filter(Species == "P. clavata" | 
           Species == "P. clavata / P. pristis",
         Zone != "Cape York QLD") %>% 
  mutate(Zone = case_when(Zone %in% c("Far North QLD", "North QLD") ~ "NEQ",
                           .default = "SEQ"))

pclist <- c("NEQ", "SEQ")
resultsc <- list()

for(i in seq_along(pclist)) {
  resultsp[[i]] <- get_ext_timelines(PC, "Zone", pclist[i],
                                     confidence_col = "Rec_Acc", timefrom = 1990)
}

ext_stbl(resultsc, pclist) 

plot_timeline(resultsc, pclist, title = "b)",
              current_year = 1990)
#cant do...