# Details ----
#' 8_fisher_trends.R
#' Paper: [Est. extent of range contraction and extinction timeline]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 2025-6-25
#' Content: loooking at gear things
#' -----------

source('helpers/help_stat.R')
source('helpers/help_news.R')
set_theme(mytheme)

library(patchwork)

EC_Date <- readxl::read_xlsx("data/xls/processed/EC_Date.xlsx") 

EC_Date %>%
  count(Cap_Clean) %>% 
  print(n = Inf)

gear_trends <- EC_Date %>% 
  mutate(
    # Fix Fisho
    Fish_Type = case_when(Fisher_Type %in% c('Pilot', 'SLS')  ~ NA_character_,
                          Fisher_Type %in% c('Com', 'Comm') |
                            str_detect(Fisher, regex('Cameron|Landenburger|Busch|
                                                     Rose|Boyd|Chaplin|Garven|Ray Marsh|
                                                     McCray|Peach|Crouch|Paddon')) | #well known fishing fams
                            Cap_Clean == "drag net" & is.na(Fisher_Type) | Cap_Clean == "trawl" ~ 'Comm',
                            Cap_Clean %in% c("cast net", "line", "fish trap") |
                            str_detect(Fisher, regex('Gibson|12-year|Parr|Davies')) ~ 'Rec',
                          .default = Fisher_Type),
    # Broader categories
    MetCap1 = case_when(
      Cap_Clean == "line" ~ "Line Fishing", 
      Cap_Clean == "cast net" ~ "Cast/Bait Net", 
      Cap_Clean == "fish trap" ~ "Fish Trap",
      Cap_Clean %in% c('set net') ~ "Set Net",
      Cap_Clean %in% c('drag net', 'seine net, boat', 'seine net, shore') ~ "Seine Net",
      Cap_Clean %in% c('net, general') ~ "Set/Seine Net (NS)",
      Cap_Clean == "trawl" ~ "Trawler",
      Cap_Clean %in% c("stabbed/shot", "roped") ~ "Targeted",
      Cap_Clean %in% c("sighting only", "stranded/predated") ~ "Non-Fishing",
      TRUE ~ "Unknown"
    )
  ) %>% 
  select(ID, Spec_Known, Year,
         Type_S, Basin, Region, Zone,
         Fish_Type, Fisher_Type, Fisher, 
         Cap_Clean, MetCap1, Fate,
         TL, LHS, Mat)

gear_trends <- gear_trends %>%
  filter(!is.na(Year), #!is.na(Cap_Clean),
         Year >= 1850, Year < 1990,
         !(Year < 1890 & Cap_Clean == "trawl"), #these are research vessels
         MetCap1 != 'Unknown') %>%
  mutate(decade = floor(Year/10)*10) 

gear_temporal <- gear_trends %>%
  count(decade, MetCap1, Fish_Type) %>%
  group_by(decade) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>% 
  mutate(
    MetCap1 = factor(MetCap1, 
                     levels = c("Non-Fishing", "Targeted", "Line Fishing", 
                                "Cast/Bait Net", "Fish Trap", 
                                "Set Net", "Seine Net", 
                                "Set/Seine Net (NS)", "Trawler")
  )) %>% 
  arrange(decade, MetCap1)

decade_totals <- gear_temporal %>%
  group_by(decade) %>%
  summarise(total_n = sum(n), .groups = "drop") #to paste n

f1 <- ggplot(gear_temporal, aes(x = decade, y = prop, 
                                fill = MetCap1)) +
  geom_col(position = "fill", width = 8) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = seq(1870, 1990, 10),
                     labels = function(x) 
                       paste0(x, "\n(n=", 
                              decade_totals$total_n[decade_totals$decade
                                                    %in% x], ")")
  ) +
  labs(
    x = "Decade",
    y = "Percent of Captures", 
    fill = "Method of Capture"
  ) +
  scale_fill_brewer(palette = 'Paired') +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
  )

EC_Date %>%
  count(Fate) %>% 
  print(n = Inf)

rls_trends <- gear_trends %>% 
  filter(Year >= 1850, Year < 1990) %>% 
  mutate(Fate_Cln = case_when(Fate %in% c("Killed", "killed", "killed, escaped") | Type_S == "Specimen" ~ "Killed",
                              Fate %in% c("Aquarium", "aquarium", "kept alive") ~ "Kept Alive (Temporary)",
                              Fate %in% c("predated", "dead") | MetCap1 == "Stranded/Dead" ~ "Stranded/Predated",
                              Fate %in% c("escaped") ~ "Escaped",
                              Fate %in% c("released") ~ "Released",
                              Fate %in% c("Sighting Only", "sighting only", "observed") ~ "Sighted",
                              .default = Fate))

rls_decade <- rls_trends %>% 
  filter(!is.na(Fate_Cln)) %>% 
  count(decade, Fate_Cln) %>%
  group_by(decade) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

f2 <- ggplot(rls_decade, aes(x = decade, y = prop,
                       fill = Fate_Cln)) +
  geom_col(position = "fill", width = 8) +  
  scale_y_continuous(labels = scales::percent) +
  scale_fill_tableau(palette = "Color Blind",
                     name = "Fate") +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank()) +
  labs(y = "Percent of Interactions")

f2 / f1 & theme(plot.margin = margin(5, 5, 5, 5),
                legend.justification = "left")

ggsave(
  "fig10.png",
  plot = last_plot(),
  device = NULL,
  path = "figs/fig10/",
  scale = 1,
  width = 9,
  height = 6,
  units = c("in", "cm", "mm", "px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
)

#statsy
kill_release <- rls_trends %>%
  filter(Fate_Cln %in% c("Killed", "Released")) %>%
  mutate(
    released = ifelse(Fate_Cln == "Released", 1, 0),
    decade_scaled = (decade - 1870) / 10  
  )

# Logistic regression
release_model <- glm(released ~ decade_scaled, family = binomial, data = kill_release)
summary(release_model)

library(DescTools)
CochranArmitageTest(table(kill_release$decade, kill_release$released))

gear_method <- gear_trends %>%
  filter(MetCap1 != "Non-Fishing",
         MetCap1 != "Cast/Bait Net") %>% #get rid of those sightings
  mutate(
    method = factor(MetCap1, levels = c("Targeted", "Line Fishing", "Fish Trap",
                                           "Set Net", "Seine Net", "Set/Seine Net (NS)", "Trawler")),
    decade_scaled = (decade - 1870) / 10  
  )

library(nnet)

method_model <- multinom(method ~ decade_scaled, data = gear_method)
summary(method_model)

library(car)
Anova(method_model)

#still goin

rls_trends %>%
  count(Saw_Kept) 

lessbias <- rls_trends %>% 
  filter(Dataset != "saws",
         Dataset != "museums",
         !is.na(Saw_Kept),
         MetCap_Cat != "Unknown",
         MetCap_Cat != "Non-Fishing",
         Fate_Cln == "Killed") 

lessbias %>% #obvs saw datasets are 100%
  count(Saw_Kept) 

chisq.test(table(lessbias$Zone, lessbias$Saw_Kept)) #mm yes interesting
chisq.test(table(lessbias$MetCap_Cat, lessbias$Saw_Kept)) #oooh

#but its prob just time!
chisq.test(table(lessbias$decade, lessbias$Saw_Kept)) #ahh there we are

trophymod1 <- glm(I(Saw_Kept == "y") ~ Year, family = binomial, data = filter(lessbias, !is.na(Saw_Kept)))
trophymod2 <- glm(I(Saw_Kept == "y") ~ Latitude, family = binomial, data = filter(lessbias, !is.na(Saw_Kept)))
trophymod3 <- glm(I(Saw_Kept == "y") ~ Year + Latitude, family = binomial, data = filter(lessbias, !is.na(Saw_Kept)))
trophymod4 <- glm(I(Saw_Kept == "y") ~ Year*Latitude, family = binomial, data = filter(lessbias, !is.na(Saw_Kept)))

summary(trophymod1)
summary(trophymod2)
summary(trophymod3)
summary(trophymod4) #worse

anova(trophymod3, trophymod2)
anova(trophymod3, trophymod1)
