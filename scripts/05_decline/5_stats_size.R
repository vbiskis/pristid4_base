# Details ----
#' 5_stats_size.R
#' Paper: [Est. extent of range contraction and extinction timeline]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 2025-06-04
#' Content: size over time 
#' Input: EC_Dens.xlsx
#' Output: TL trends
#' + ind year map 
#' + dfs for main fig 8
#' -----------

source('helpers/help_stat.R')
EC_Dens <- readxl::read_xlsx("data/xls/processed/EC_Dens.xlsx") 

EC_Dens <- EC_Dens %>% 
  mutate(LHS = case_when(LHS %in% c('neonate', 'YOY') ~ 'YOY',
                         TL >= 350 ~ 'adult',
                         .default = LHS),
         Mat = case_when(LHS == 'adult' ~ "mature",
                              .default = Mat),
         Zone = factor(Zone,
                       levels = c('Cape York QLD', 'Far North QLD',
                                    'North QLD', 'Central QLD',
                                    'Wide Bay QLD', 'South East QLD',
                                    'North Coast NSW', 'Mid Coast NSW')))

EC_nobabies <- EC_Dens %>% 
  filter(LHS != 'YOY' | is.na(LHS))

#Size - yo fave!----
## first look----
ggplot(data = EC_nobabies,
       aes(x = Year, y = TL)) +
  geom_point(aes(color = Spec_Known)) +
  geom_smooth(method = "gam", color = "black") + #ahhh i see
  scale_color_manual(values = species_colors)

summary(lm(log(TL) ~ Year, data = EC_nobabies)) #she's sig! even with log

## to report: largest per year----
SizeTrends <- EC_Dens %>% 
  filter(TL >= 200 | Mat == 'adult') %>% # remove juveniles, supposed to capture the largest
  dplyr::mutate(Decade = floor(Year/10)*10) %>% 
  group_by(Zone, Region, Basin, Year) %>% 
  summarise(
    Lmax = ifelse(all(is.na(TL)), NA, max(TL, na.rm = TRUE)),
    specbig = Spec_Known[which.max(TL)],
    count = sum(Abund[Abund > 1], na.rm = TRUE),
    n = n()
  )

PupTrends <- EC_Dens %>% 
  dplyr::mutate(Decade = floor(Year/10)*10) %>% 
  group_by(Zone, Region, Basin, Year) %>% 
  summarise(
    pups = sum(Num_Pups > 0, na.rm = TRUE),
    spec_pup1 = if_else(any(Num_Pups > 0, na.rm = TRUE), 
                        Spec_Known[which(Num_Pups > 0)[1]], 
                        NA_character_),
    ispup = sum(Age <= 1 | LHS == "YOY", na.rm = TRUE),
    spec_pup2 = if_else(any(Age <= 1 | LHS == "YOY", na.rm = TRUE), 
                        Spec_Known[which(Age <= 1 | TL < 130 | LHS == "YOY")[1]], 
                        NA_character_),
    pupup = ispup + pups,
    specpup = if_else(!is.na(spec_pup1), spec_pup1, spec_pup2)
  ) %>% 
  dplyr::select(-spec_pup1, -spec_pup2)

# filter and save----
SizeTrendDat <- SizeTrends %>% 
  filter(Zone != "Mid Coast NSW" & 
           Zone != "Cape York QLD") %>%  #better
  mutate(specbig = factor(specbig,
                          levels = c('A. cuspidata',
                                     'P. clavata',
                                     'P. pristis',
                                     'P. zijsron', 
                                     'Pristidae')))

PupTrendDat <- PupTrends %>% 
  filter(Zone != "Mid Coast NSW" & 
           Zone != "Cape York QLD") %>%  #better
  mutate(specpup = as.factor(specpup))

write_xlsx(SizeTrendDat, "data/xls/processed/sizetrends.xlsx")
write_xlsx(PupTrendDat, "data/xls/processed/puptrends.xlsx")

ggplot(SizeTrendDat, aes(x = Year)) +
  geom_point(aes(y = Lmax,
                 color = specbig), 
             size = 0.5) +
  geom_smooth(aes(y = Lmax), 
              method = "lm", 
              color = "black", size = 0.5) +
  facet_wrap(~ Zone, scales = "free_x") +
  geom_col(data = SizeTrendDat,
           aes(y = n*100), 
           alpha = 0.5, 
           position = "stack") +
  geom_col(data = PupTrendDat %>% 
             filter(!is.na(specpup)),
                    aes(y = pupup*100, fill = specpup), 
           alpha = 0.75,
           position = "stack") +
  scale_y_continuous(
    name = "Maximum TL (cm)",
    sec.axis = sec_axis(~.*1/100, name = "Count")
  ) +
  scale_color_manual(values = species_colors, name = "Species") +  
  scale_fill_manual(values = species_colors, name = "Species")  +
  theme_clean() +
  guides(color = guide_legend(title = "Species"),
         fill = guide_legend(title = "Species", 
                             override.aes = list(alpha = 1))) + #ugh why
  theme(
    legend.position = "bottom"
  ) 

#ahhh yes diff results without outliers, let's be cautious
