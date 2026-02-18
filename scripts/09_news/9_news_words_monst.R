# Details ----
#' 9_news_words_monst.R
#' Paper: [Est. extent of range contraction and extinction timeline]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 2025-12-3
#' Content: word analysis - just monster
#' + Stats only
#' -----------

source('helpers/help_news.R')

trove_main <- readxl::read_xlsx('data/xls/processed/trove_main.xlsx')
arts_wsnt <- readRDS('data/rds/arts_wsnt.rds')
art_sent <- readRDS('data/rds/art_sent.rds')

arts_m <- art_sent %>% 
  filter(word == "monster",
         sentiment == "fear") #204 times

arts_m %>% 
  summarise(
    art_type = n_distinct(TVN), #188
    events = n_distinct(SFN) #73
  )

monsters <- trove_main %>%
  filter(str_detect(`Full text`, regex("monster", ignore_case = TRUE)) | 
        str_detect(Art_Title, regex("monster", ignore_case = TRUE)))

monsters %>%
  summarise(
    arts = n_distinct(TVN), #232
    events = n_distinct(SFN), #77
    total_in_text = sum(str_count(`Full text`, regex("monster", ignore_case = TRUE)), na.rm = TRUE),
    total_in_title = sum(str_count(Art_Title, regex("monster", ignore_case = TRUE)), na.rm = TRUE),
    total_mentions = total_in_text + total_in_title
  )

setdiff(monsters$SFN, arts_m$SFN) #ugh sort later I guess
setdiff(monsters$SFN, trove_m_arts$SFN) #oh! okay great
setdiff(monsters$SFN, tidy_news$SFN) 
setdiff(monsters$SFN, art_sent$SFN) #only when filtering out 1860's
#k down to 3 unexplained
# oh! ITS PLURALS! *fixed
# + one monster in the title

monsters <- monsters %>% 
  mutate(
    SFN = ifelse(SFN == "TRV0207/8", "TRV0278", SFN)
  )

#k great!

trove_main %>%
  summarise(
    arts = n_distinct(TVN), #232
    events = n_distinct(SFN))
  
monst_props <- trove_main %>% 
  mutate(
    ismon = case_when(SFN %in% monsters$SFN ~ 1,
                      .default = 0)
  )

sfn_level <- monst_props %>%
  group_by(SFN) %>% 
  summarise(
    ismon = as.numeric(any(ismon == 1)),
    year = first(Year_Art)
  )

sfn_level <- sfn_level %>%
  mutate(decade = floor(year / 10) * 10) %>% 
  filter(year < 1960)

mprops <- sfn_level %>%
  group_by(decade) %>%
  summarise(
    total = n(),
    n_monster = sum(ismon),
    prop_monster = mean(ismon)
  )

model <- glm(ismon ~ decade, data = sfn_level, 
             family = binomial)
summary(model)
plot(model)

ggplot(mprops, aes(x = decade, y = prop_monster)) +
  geom_line() +
  geom_point(aes(size = total)) +
  scale_y_continuous(labels = scales::percent) +
  labs(y = "% of Events with Monster", 
       x = "Decade",
       size = "Total Events") +
  theme_minimal()
