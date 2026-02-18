# Details ----
#' 2_appendix_timeline.R
#' Paper: [Est. extent of range contraction and extinction timeline]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 2025-05-04
#' Content: for supps: a visual of all historical sawfish interactions
#' + show breakdown of confirmed/etc
#' 
#' Output: appendix figure (app_s4.png/tiff)
#' + separate table of all data sources for appendix (incoming)
#' -----------

pacman::p_load("ggplot2", 
               "dplyr",
               "readxl")

#want all dates for full visual
EC_Spec <- readxl::read_xlsx("data/xls/processed/EC_Spec.xlsx") 

# Notes----
# GoC is just a bunch of baby PPs
# not a useful rep with the dist of newspapers

#1 - Data Source Dist ----
#make a diff cutoff from drainage (that's just state): extent
##for easy vis.----
EC_TL <- EC_Spec %>% 
  mutate(CurrRange = case_when(mean_lat > -23.25 ~ "Extant",
                               .default = "Historic"),
         Rec_Acc = factor(Rec_Acc, levels = c("1", "2", "3", "0")),
         #idk why when i reload it in it needs me to do this again
         Basin = fct_reorder(Basin, mean_lat), 
         Region = fct_reorder(Region, mean_lat)) %>% 
  filter(!is.na(Basin))

#As Timeline:----
sourceplot <- ggplot(EC_TL, aes(x = Year, y = Basin)) +
  geom_errorbarh(aes(xmin = case_when(
    Date_Acc == 1 ~ Year,      # Exact day - no uncertainty
    Date_Acc == 2 ~ Year - 0.5, # Within year - show half year uncertainty
    Date_Acc == 3 ~ Year - 5    # ±5 years
  ),
  xmax = case_when(
    Date_Acc == 1 ~ Year,      
    Date_Acc == 2 ~ Year + 0.5, 
    Date_Acc == 3 ~ Year + 5    
  ),
  color = factor(Rec_Acc)),
  alpha = 0.3, height = 0.2) +
  geom_point(aes(color = factor(Rec_Acc),
                 shape = Data_Met), 
             alpha = 0.7, size = 2) +
  scale_color_manual(
    name = "Sawfish Presence",
    values = c("1" = "#33A02C",
               "2" = "#1F78B4",
               "3" = "#FF7F00",
               "0" = "darkred"),
    labels = c("1" = "Photographed",
               "2" = "Documented",
               "3" = "Probable",
               "0" = "Unconfirmed"))  +
  facet_wrap(~ CurrRange, 
             ncol = 2, scales = "free") +
  labs(
    title = "",
    x = "Year",
    y = "Drainage Basin",
    shape = "Data Source"
  ) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10),
        strip.text = element_blank())

#As Grid:----
##prep----
EC_TL <- EC_TL %>% #need decades here... too many years
  mutate(Decade = floor(Year/10)*10) %>%
  group_by(Decade, Basin) %>%
  mutate(has_conf = any(Spec_Conf == 1),
         has_unID = any(Spec_Conf == 2)) %>% #add some fun things in!
  ungroup()

##alt plot----
sourceplot2 <- ggplot(EC_TL %>% 
         mutate(Dec_Print = case_when(Decade == 1800 ~ 1850,
                                      .default = Decade)), #just for this graph, don't need to have in the actual df
       aes(x = Dec_Print, y = Basin)) +
  geom_tile(aes(fill = factor(Data_Met)), 
            color = "white", linewidth = 0.5, alpha = 0.5) +
  geom_text(aes(label = case_when(
    Spec_Conf == 3 & !has_conf & !has_unID ~ "X", #only when there is no confirmed
    Spec_Conf == 2 & !has_conf ~ "O", #only when there is no confirmed
    TRUE ~ ""                 # Confirmed - no symbol
  )), 
  color = "black", size = 3, fontface = "bold") +
  scale_fill_manual(
    name = "Data Source",
    values = c("News/Media" = "red3",    # Green for news
               "Public Institution" = "yellow2", #Deep red for museums idk why
               "SARA Database" = "deepskyblue3"),   # And Duh SARA Blue!
    na.value = "grey90") +
  facet_wrap(~ CurrRange, #just to see it while making, but should be fine
             ncol = 2, scales = "free") +
  labs(x = "Year", y = "Drainage Basin", fill = "Data Source") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank(),
    strip.text = element_text(face = "bold")
  ) +
  scale_x_continuous(
    breaks = c(1850, 1860, 1870, 1880, 1890, 1900, 1910, 1920, 1930, 1940, 1950, 1960, 1970, 1980, 1990, 2000, 2010, 2020),
    labels = c("1800", "//", "1870", "1880", "1890", "1900", "1910", "1920", "1930", "1940", "1950", "1960", "1970", "1980", "1990", "2000", "2010", "2020")
  ) 

#k fig s1:----
ggsave("figs/s4/app_s4.png",
  plot = sourceplot,
  device = NULL,
  path = NULL,
  scale = 1,
  width = 12,
  height = 12,
  units = c("in", "cm", "mm", "px"),
  #dpi = 300,
  limitsize = TRUE,
  bg = NULL,
)
