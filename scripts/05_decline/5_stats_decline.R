# Details ----
#' 5_stats_decline.R
#' Paper: [Est. extent of range contraction and extinction timeline]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 2025-06-03
#' Content: 
#' Input: EC_Date.xlsx
#' Output: initial tests/filtering for gam
#' -----------

library(writexl)
source('helpers/help_stat.R')
EC_Dens <- readxl::read_xlsx("data/xls/processed/EC_Dens.xlsx") 

# Abundance Trends----
EC_freqs <- EC_Dens %>% 
  group_by(Zone, Region, Basin, Year) %>% 
  summarise(
    Abu = sum(Abund, na.rm = TRUE),
    Catchfreq = n(),
    meanTL = mean(TL, na.rm = TRUE),
    maxTL = ifelse(all(is.na(TL)), NA, max(TL, na.rm = TRUE)),
    meanPups = mean(Num_Pups, na.rm. = TRUE)
  )

EC_freqs %>%
  group_by(Zone) %>% 
  summarise(
    meanCatch = mean(Catchfreq, na.rm = TRUE),
    sdCatch = sd(Catchfreq, na.rm = TRUE)
  )

#too infrequent
kruskal.test(Catchfreq ~ Zone, data = EC_freqs) #mmm nope

## grouping by decade:----
EC_Dec <- EC_Dens %>% 
  dplyr::mutate(Decade = floor(Year/10)*10) %>% 
  group_by(Zone, Region, Decade) %>% 
  summarise(
    Abu = sum(Abund, na.rm = TRUE),
    Catchfreq = n(),
    meanTL = mean(TL, na.rm = TRUE),
    maxTL = ifelse(all(is.na(TL)), NA, max(TL, na.rm = TRUE)),
    meanPups = mean(Num_Pups, na.rm = TRUE)
  ) %>% 
  mutate(
    Zone = factor(Zone,
                  levels = c('Cape York QLD', 'Far North QLD',
                             'North QLD', 'Central QLD',
                             'Wide Bay QLD', 'South East QLD',
                             'North Coast NSW', 'Mid Coast NSW'))
  )

write_xlsx(EC_Dec, 'data/xls/processed/EC_Dec.xlsx') #for decade trends

## check it out----
EC_Dec %>%
  group_by(Zone) %>% 
  summarise(
    n = n(),
    meanCatch = mean(Catchfreq, na.rm = TRUE),
    sdCatch = sd(Catchfreq, na.rm = TRUE)
  ) %>% 
  arrange(desc(meanCatch), Zone)

ggplot(EC_Dec) +
  geom_histogram(aes(x = Catchfreq)) +
  facet_grid(~ Zone) #mmmm remove our small guys

kruskal.test(Catchfreq ~ Zone, data = EC_Dec) #seems to be!

#let's buld a model!