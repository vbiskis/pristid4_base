# Details ----
#' 9_words_correlation.R
#' Paper: [Est. extent of range contraction and extinction timeline]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 2025-12-1
#' Content: word analysis - send correlations
#' + Stats only
#' -----------

source('helpers/help_news.R')

arts_wsnt <- readRDS('data/rds/arts_wsnt.rds')
art_sent <- readRDS('data/rds/art_sent.rds')

library(psych)
library(corrplot)
library(stats)

# word trends ----
## correlations-----

# gotta average by SFN for scaling

arts_sfn <- arts_wsnt %>% 
  filter(Year_Art < 1960) %>% 
  group_by(SFN) %>% #for duplicates from the big cities
  summarise(
    emot = mean(positive_pct + negative_pct, na.rm = TRUE),
    tot_emot = mean(positive + negative, na.rm = TRUE),
    meanp = mean(positive_pct, na.rm = TRUE), #positive
    sdp = sd(positive_pct, na.rm = TRUE),
    meann = mean(negative_pct, na.rm = TRUE), #negative
    sdn = sd(negative_pct, na.rm = TRUE),
    meanf = mean(fear_pct, na.rm = TRUE), #fear
    sdf = sd(fear_pct, na.rm = TRUE),
    meana = mean(surprise_pct, na.rm = TRUE), #surprise
    sda = sd(surprise_pct, na.rm = TRUE),
    meanm = mean(anger_pct, na.rm = TRUE), #anger
    sdm = sd(anger_pct, na.rm = TRUE),
    meand = mean(disgust_pct, na.rm = TRUE), #disgust
    sdd = sd(disgust_pct, na.rm = TRUE),
    means = mean(net_sent, na.rm = TRUE), #net
    sds = sd(net_sent, na.rm = TRUE),
    year = min(Year_Art), #year
    TL = mean(TL_final, na.rm = TRUE), #tl
    Period = first(Period),
    n_art = first(n_Art),
    total = mean(total)
  )

saveRDS(arts_sfn, 'data/rds/arts_sfn.rds')

#just for sentimental and fear?
cor_matrix <- cor(arts_sfn %>% 
                    dplyr::select(where(is.numeric)),
                  method = "spearman", use = 'complete.obs')

p_matrix <- cor.mtest(arts_sfn %>% 
                        select(where(is.numeric)), 
                      method = "spearman")

corrplot(cor_matrix, method = "circle", type = "upper", 
         addCoef.col = "black", number.cex = 0.7,
         tl.cex = 0.8, tl.col = "black",
         p.mat = p_matrix$p,      # Add p-values
         sig.level = 0.05,        # Significance threshold
         insig = "blank")     

## regression check----

#ones to report!
ggqqplot(arts_sfn$emot) # hes probs fine but well go kw anywya
ggqqplot(arts_sfn$meanf) # lots of 0's again, tweedie?
ggqqplot(arts_sfn$means) # this guy is pretty normal too
ggqqplot(arts_sfn$TL) #noooo

# important; corr with tot_emot and total absolutely linear
plot(arts_sfn$year, arts_sfn$emot) # oh there is a dip tho
plot(arts_sfn$year, arts_sfn$meanf) # we see a dip again
plot(arts_sfn$TL, arts_sfn$means) # more neg later it seems
plot(arts_sfn$TL, arts_sfn$meanf) # woah that looks quad!

#check interactions
arts_sfn <- arts_sfn %>% 
  mutate(Period = factor(Period, levels = c("Pre-1900", "1901-1915",
                                            "1916-1930", "1931-1945", 
                                            "1946-1960")))

fear_linear <- lm(meanf ~ TL, data = arts_sfn)
fear_quad <- lm(meanf ~ TL + I(TL^2), data = arts_sfn)
fear_wper <- lm(meanf ~ TL + Period, data = arts_sfn)

library(performance)
check_model(fear_wper) # dont really like any of these - not normal

library(gamlss)
arts_sfn$f_prop <- arts_sfn$meanf / 100

# Zero-inflated Beta (allows exactly 0)
fear_zero <- gamlss(f_prop ~ TL + year,
                family = BEINF,  # Beta Inflated at 0 (and 1)
                data = na.omit(arts_sfn)) #not as 

plot(fear_zero) #looks good but
summary(fear_zero) #none of those trends sig! nvm then

# report stats----
library(broom)

arts_sfn %>%
  do(tidy(t.test(.$meann, .$meanp, paired = TRUE)))

# get some numbers----
arts_wsnt %>%
  group_by(Period) %>%
  summarise(
    p = mean(positive, na.rm = TRUE),
    ps = sd(positive, na.rm = TRUE),
    n = mean(negative, na.rm = TRUE),
    ns = sd(negative, na.rm = TRUE),
    f = mean(fear, na.rm = TRUE),
    fs = sd(fear, na.rm = TRUE),
    total = mean(positive + negative, na.rm = TRUE),
    totsd = sd(positive + negative, na.rm = TRUE)
  ) #for raw reporting

# looks like its a non para for meeee
emotmod <- aov(emot ~ Period, data = arts_sfn)
summary(emotmod)
kruskal.test(emot ~ Period, data = arts_sfn) #still very much yes
summary(aov(means ~ Period, data = arts_sfn)) # nope
kruskal.test(meanf ~ Period, data = arts_sfn) # yes

# Post-hoc pairwise comparisons
emm <- emmeans(emotmod, ~ Period)
pairs(emm, adjust = "tukey") 

arts_sfn %>% 
  group_by(Period) %>%
  summarise(
    e = mean(tot_emot, na.rm = TRUE),
    s = sd(tot_emot, na.rm = TRUE)
  )

pairwise.wilcox.test(arts_sfn$emot, arts_sfn$Period, 
                     p.adjust.method = "bonferroni", exact = FALSE)

pairwise.wilcox.test(arts_sfn$meanf, arts_sfn$Period, #but actually no
                     p.adjust.method = "bonferroni", exact = FALSE)

# and plot----
arts_year <- arts_sfn %>% 
  group_by(year) %>% #for duplicates from the big cities
  summarise(
    meanp = mean(meanp, na.rm = TRUE), #positive
    sdp = sd(meanp, na.rm = TRUE),
    meann = mean(meann, na.rm = TRUE), #negative
    sdn = sd(meann, na.rm = TRUE),
    meanf = mean(meanf, na.rm = TRUE), #fear
    sdf = sd(meanf, na.rm = TRUE),
    meana = mean(meana, na.rm = TRUE), #surprise
    sda = sd(meana, na.rm = TRUE),
    means = mean(means, na.rm = TRUE), #net
    sds = sd(means, na.rm = TRUE),
    n = sum(n_art, na.rm = TRUE)
  )

#wait!
ggplot(data = arts_year) +
  geom_col(aes(y = -meann, x = year),
           fill = "grey75", width = 1,
           linewidth = 0.5, color = "black") +
  geom_col(aes(y = meanp, x = year),
           fill = "grey75", width = 1, 
           linewidth = 0.5, color = "black") +
  geom_col(aes(y = -meanf, x = year),
           fill = met.brewer("Monet")[1], width = 1, color = "black") +
  pilot::theme_pilot(grid = "", axes = "b") +
  labs(x = 'Year', y = 'Average Sentiment Term Percentage (%)')

#shes beautiful! but shes not adding what I need

