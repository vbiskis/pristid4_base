# Details ----
#' 9_news_stats.R
#' Paper: [Est. extent of range contraction and extinction timeline]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 2025-6-14
#' Content: get extinction values (xls)
#' + Figure 9!
#' + This one needs to be edited prior to Git for brevity, but for now fine.
#' -----------

source('helpers/help_stat.R')
source('helpers/help_news.R')
set_theme(mytheme)

trove_all <- readxl::read_xlsx("data/xls/TroveDat_ALLArts.xlsx") 
trove <- readxl::read_xlsx("data/xls/News_SU.xlsx") #news

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

# News Bias----
trove_tests <- trove_all_sfo %>% 
  left_join(trove %>% 
              mutate(Region2 = case_when(Basin == "JOHNSTONE RIVER" | Basin == "MURRAY RIVER" |
                                           Basin == "HERBERT RIVER" | Basin == "HINCHINBROOK ISLAND" |
                                           Basin == "TULLY RIVER" ~ "JOHNSTONE",
                                         Basin == "BLACK RIVER" | Basin == "ROSS RIVER" ~ "BURDEKIN",
                                         Basin == "BAFFLE CREEK" ~ "BURNETT",
                                         Basin == "MAROOCHY RIVER" ~ "BRISBANE",
                                         Basin == "TWEED RIVER" ~ "GOLD COAST",
                                         Basin == "BRUNSWICK RIVER" | Basin == "RICHMOND RIVER" |
                                           Basin == "CLARENCE RIVER" ~ "NORTHERN RIVERS",
                                         Basin == "BELLINGER RIVER" | Basin == "MACLEAY RIVER" |
                                           Basin == "HASTINGS RIVER" | Basin == "MANNING RIVER" ~ "COFFS COAST",
                                         .default = Region)) %>% #fix the weird ones
              dplyr::select(SFN, Cap_Rural, Basin, Region2)) %>%  #getting correct regions of actual catches
  dplyr::select(-Age, -TL_final, -TL_ImgJ) %>%  #use TL_cm col
  mutate(LocalR = case_when(NP_Region == Region2 ~ 1,
                            .default = 0),
         LocalB = case_when(NP_Basin == Basin ~ 1,
                            .default = 0))

#big questions:

# metro v rural (just going on capitols for now)----

#reporting biases - first look:

trove_tests %>% 
  group_by(NP_Rural) %>% 
  summarise(
    PicArt = sum(PicID),
    meanTL = mean(Size_cm, na.rm = TRUE),
    LocR = mean(LocalR, na.rm = TRUE),
    LocB = mean(LocalB, na.rm = TRUE)
  )

## number re reports----
#seems rural less papers, but regional/metro just as likely
#where reporting?

fisher.test(table(trove_tests$NP_Rural, trove_tests$LocalR))
chisq.test(trove_tests$NP_Rural, trove_tests$LocalR) #significant

fisher.test(table(trove_tests$NP_Rural, trove_tests$LocalB))
chisq.test(trove_tests$NP_Rural, trove_tests$LocalB) #and again

local_summary <- trove_tests %>%
  group_by(SFN) %>%
  summarise(
    reports = n(),
    is_local = max(LocalR), # 1 if any reports were local
    .groups = "drop"
  )

wilcox.test(reports ~ is_local, data = local_summary) #ah yes!

#what about whether the catch was rural-metro-regional?

catch_counts <- trove_tests %>%
  group_by(SFN, Cap_Rural) %>%  # Cap_Rural from the catch data
  summarise(n_Art = n(), .groups = "drop")

anova(glm(n_Art ~ Cap_Rural, family = "poisson", data = catch_counts))

catch_counts %>% 
  group_by(Cap_Rural) %>% 
  summarise(n = n(),
            meanrep = mean(n_Art, na.rm = TRUE),
            sdrep = sd(n_Art, na.rm = TRUE))

#ah yes its def about metro coverage for sureeee

## page number----
colnames(trove_tests)

#gross
library(stringr)

trove_tests <- trove_tests %>%
  mutate(
    NP_pg_cln = case_when(
      is.na(NP_pg) ~ NA_real_,
      str_detect(NP_pg, ",") ~ as.numeric(str_extract(NP_pg, "^\\d+")), # First number before comma
      TRUE ~ as.numeric(NP_pg)
    )
  ) %>% 
  filter(NP_pg_cln <= 50)

anova(lm(NP_pg_cln ~ NP_Rural, data = trove_tests)) #even when removing wild page numbers!

trove_tests %>%
  group_by(NP_Rural) %>%
  summarise(
    n = n(),
    mean_page = mean(NP_pg_cln, na.rm = TRUE),
    sd_page = sd(NP_pg_cln, na.rm = TRUE)
  ) #cool!

## size----
catch_summary <- trove_tests %>%
  group_by(SFN) %>%
  summarise(
    report_count = n(),
    size = mean(Size_cm, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(!is.na(size))

cor.test(catch_summary$size, catch_summary$report_count, method = "spearman") #doesnt look like it
summary(glm(report_count ~ size, family = "poisson", data = catch_summary))
#nope not it

cor.test(trove_tests$Size_cm, trove_tests$NP_pg_cln, use = "complete.obs")
summary(lm(NP_pg_cln ~ Size_cm, data = trove_tests))

#okay! yes to size influencing the number of reports + page number - 
npglm <- glm(log(NP_pg_cln) ~ log(Size_cm) + NP_Rural*LocalR + Year_Art,
            data = trove_tests)

par(mfrow = c(2,2))
plot(npglm)
summary(npglm) #but page number is more affected by region

## pic----

sitsonly <- trove_tests %>% 
  filter(ArtType == "Sawfish capture/sighting") %>% 
  mutate(Picture = as.factor(PicID)) #bc not really in other cats

sitsonly %>% 
  group_by(Picture, NP_Rural) %>% 
  summarise(
    meanTL = mean(Size_cm, na.rm = TRUE),
    sdTL = sd(Size_cm, na.rm = TRUE),
    n = n()
  ) #slightly yes

t.test(Size_cm ~ Picture, data = sitsonly) #awesome

ggplot(sitsonly) +
  geom_point(aes(x = Size_cm, y = NP_pg_cln,
                 color = Picture),
             size = 1) +
  facet_grid(LocalR ~ NP_Rural) +
  geom_smooth(aes(x = Size_cm, y = NP_pg_cln),
              method = "lm", color = "black") +
  scale_color_manual(name = "Picture Present",
                     values = c("grey", "darkgreen"),
                     labels = c("No", "Yes")) +
  scale_y_log10() + 
  labs(
    x = "Total Length Reported (cm)",
    y = "Page in Newspaper"
  ) +
  theme(
    strip.background.x = element_rect(color = "grey50", linewidth = 1),
    strip.text.x = element_text(margin = margin(t = 5, b = 5)),
    strip.text.y = element_blank(),
    legend.position = "bottom",
    plot.margin = margin(5,5,5,5)
  )

ggsave(
  "fig11.png",
  plot = last_plot(),
  device = NULL,
  path = "figs/fig11/",
  scale = 1,
  width = 6,
  height = 5,
  units = c("in", "cm", "mm", "px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
)
