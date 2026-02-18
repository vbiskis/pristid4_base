# Details ----
#' 9_news_words_get.R
#' Paper: [Est. extent of range contraction and extinction timeline]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 2025-12-1
#' Content: prep for word analysis: get word counts
#' + Figure 12
#' -----------

source('helpers/help_news.R')
trove_m_arts <- readxl::read_xlsx("data/xls/processed/trove_m_arts.xlsx") 

# quick filter----
tidy_news <- trove_m_arts %>%
  unnest_tokens(word, `Full text`) %>%   
  anti_join(stop_words)  # Remove common words like "the" etc.

tidy_title <- trove_m_arts %>%
  unnest_tokens(word, Art_Title) %>%   
  anti_join(stop_words) #but also can modify for words that don't hold that meaning

art_w_edit <- tidy_news %>% #merge commons
  mutate(word = case_when(
    word %in% c("drag", "dragged", "damaging") ~ "drag",
    word %in% c("monster", "monsters") ~ "monster",
    word %in% c("damage", "damaged", "damages", "damaging") ~ "damage",
    word %in% c("excite", "excitement", "exciting") ~ "excitement",
    word %in% c("danger", "dangerous", "dangerously") ~ "danger",
    word %in% c("kill", "killed", "killing", "kills") ~ "kill",
    word %in% c("struggle", "struggled") ~ "struggle",
    word %in% c("favorite", "favourite") ~ "favourite",
    word %in% c("thrashing", "thrash", "thrashed") ~ "thrash",
    word %in% c("combatants", "combatant") ~ "combatant",
    word %in% c("endeavor", "endeavour") ~ "endeavour",
    word %in% c("favour", "favorable", "favourable") ~ "favourable",
    word %in% c("succeed", "success", "succeeded", "successfully") ~ "success",
    word %in% c("attack", "attacked", "attacking", "attacks") ~ "attack",
    TRUE ~ word  # Keep original if not in list
  ))

# get tone----

# First, load a sentiment lexicon
nrc_sent <- get_sentiments("nrc")

# Join with tidy data
art_sent <- art_w_edit %>%
  inner_join(nrc_sent, by = "word") %>% 
  filter(!(word %in% c("shallow", "lying", "top", "lower", "mouth", "forward", "bottom", # directions
                       "sea", "coast", "land", 'tree', "spit", 'beach', "tower", # locations - gen
                       "rock", "real", "hale", "pacific", "mediterranean", "buoy", "building",
                       "cane", "change", "government", "kerosene", "tributary", "pool", "occupation",
                       "olfactory", "cherry", "ken", "soup", "blue", "naturalist", "ply",
                       "pilot", "flying", "manly", "standing", "harry", # locations - place name
                       "pine", "swim", "league", "included", "information", "feature", "library",
                       "drag", "gear", "bait", 'landed', "harbor", 'hooked', 'shark', # fishing neutral
                       "anchor", "anchorage", 'launch', "catch", "john", "margin",
                       "time", "august", "paddle", "payment", "gap", "stern", "tabacco", "row",
                       'engaged', 'birth', 'baby', "nurse",  #not what it thinks!
                       "weigh", "weight", "length", "measured", "larger", "considerable", # measuring
                       "owing", "deal", "including", "found", "possession", # ownership - neutral
                       'contact', "radio", "talk", 'experienced', 'bath'))) # stop words
  
common_words <- art_sent %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

unique(art_sent[art_sent$sentiment == "negative", ]$word)

common_words %>%
  group_by(sentiment) %>%
  slice_max(n, n = 15) %>% 
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Contribution to sentiment",
       y = NULL)

#blech these are trash----
trust_words <- c("author", "advised", "accommodation", "dear", "result",
                 "advocate", "crew", "marine", "official", "horse", 
                  "school", "food", "series", "money", "pay", "priest", 
                  "policeman", "father", "police", "shoulder", "remains", 
                   "base", "dictionary", "cap", "employ", "courier", 
                   "bank", "iron", "rod", "coach", "account", "majority", 
                   "church", "green", "wear", "mother", "cover", 
                   "feeling", "harbor", "sir", "smith", "badge", "teacher", 
                   "council", "rule", "measure", "ambulance", "depth", "ground",
                   "chronicle", "doctor", "usual", "symmetry", "witness", 
                   "routine", "operation", "management", "commission",
                   "constable", "theory", "sun", "don", "fill", "level", 
                   "star", "officer", "fixed", "principal", "count", "picnic",
                   "committee", "association", "shelter", "gate", 
                   "truck", "related", "lord", "proof")

art_sent <- art_sent%>%
  filter(!word %in% trust_words)

common_words <- art_sent %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup() #make again

#lets just check 

test <- art_sent %>% 
  filter(TVN == 46690024) #very neg ex: 117326552 | very pos ex: 162157407

# get counts----

#check for both cats
out_of_bounds <- art_sent %>%
  filter(sentiment %in% c("negative", "positive")) %>% 
  distinct(word, sentiment) %>%
  count(word) %>%
  filter(n > 1) %>%  # appears in both sentiments
  pull(word)

art_sent <- art_sent %>% # sentiment counts
  filter(!(word %in% c('escape', 'armed', 'spectacle') & sentiment == 'positive'),
         !(word == 'supremacy' & sentiment == 'negative')) # code for just one
                         
sent_counts <- art_sent %>% 
  count(TVN, sentiment) %>%
  tidyr::pivot_wider(names_from = sentiment, 
                     values_from = n, values_fill = 0) %>%
  mutate(
    net_sentiment = positive - negative
  )

total_words <- trove_m_arts %>% # total words
  unnest_tokens(word, `Full text`) %>%  # 
  group_by(TVN) %>%
  summarize(total = n())

# merge in----
trove_m_arts <- trove_m_arts %>%
  left_join(total_words)

sentiment_cols <- c("anger", "anticipation", "disgust", "fear", "joy", 
                    "sadness", "surprise", "negative", "positive")

arts_wsnt <- trove_m_arts %>%
  mutate(decade = floor(Year_Art / 10) * 10) %>%
  left_join(sent_counts, by = "TVN") %>% 
  mutate(
    across(all_of(sentiment_cols), 
           ~.x / total * 100, 
           .names = "{.col}_pct"),
    net_sent = net_sentiment / total,
    NP_pg = as.numeric(NP_pg)
  ) 

# and let's save these for stats
saveRDS(arts_wsnt, 'data/rds/arts_wsnt.rds')
saveRDS(art_sent, 'data/rds/art_sent.rds')
