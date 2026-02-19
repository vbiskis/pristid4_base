# Details ----
#' 5_fig8_absize.R
#' Paper: [Est. extent of range contraction and extinction timeline]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 2025-06-04
#' Content: size over time 
#' Input: EC_Dens.xlsx
#' Output: TL v abundance trends - decade
#' + fig 8
#' -----------

source('helpers/help_stat.R')
# set_theme(mytheme) #not actually the best for this one!

library(ggthemes)

EC_Dens <- readxl::read_xlsx("data/xls/processed/EC_Dens.xlsx") 
SizeTrendDat <- readxl::read_xlsx("data/xls/processed/sizetrends.xlsx") 
PupTrendDat <- readxl::read_xlsx("data/xls/processed/puptrends.xlsx") 

SizeTrendDat <- SizeTrendDat %>% 
  mutate(Zone = factor(Zone,
                       levels = c('Far North QLD',
                                  'North QLD', 'Central QLD',
                                  'Wide Bay QLD', 'South East QLD',
                                  'North Coast NSW'))) 

PupTrendDat <- PupTrendDat %>% 
  mutate(Zone = factor(Zone,
                       levels = c('Far North QLD',
                                  'North QLD', 'Central QLD',
                                  'Wide Bay QLD', 'South East QLD',
                                  'North Coast NSW'))) 
  
EC_Dens <-  EC_Dens %>% 
  mutate(Zone = factor(Zone,
                       levels = c('Far North QLD',
                                  'North QLD', 'Central QLD',
                                  'Wide Bay QLD', 'South East QLD',
                                  'North Coast NSW'))) %>% 
  filter(Zone != "Mid Coast NSW" & 
           Zone != "Cape York QLD") 

# final graph together----
## prep species comp----

EC_props <- EC_Dens %>%
  dplyr::mutate(Decade = floor(Year/10)*10) %>% 
  group_by(Zone, Decade, Spec_Known) %>%  
  summarise(count = n(), .groups = "drop") %>%  
  group_by(Zone, Decade) %>%  
  mutate(prop = count / sum(count)) %>%
  ungroup()

## prep total frequencies
EC_freqs <- EC_Dens %>% 
  dplyr::mutate(Decade = floor(Year/10)*10) %>% 
  group_by(Zone, Decade) %>% 
  summarise(
    Catchfreq = n()
  )

## make plot----
max_y_by_zone <- SizeTrendDat %>%
  group_by(Zone) %>%
  summarise(max_y = max(Lmax, na.rm = TRUE) * 1.1)  # Add 10% buffer

ggplot(SizeTrendDat) +
  facet_wrap(~ Zone, 
             nrow = 2, scales = "free_y") +
  geom_tile(data = EC_props %>%
              left_join(SizeTrendDat %>% 
                          group_by(Zone) %>% 
                          summarise(max_y = max(Lmax, na.rm = TRUE)), 
                        by = "Zone") %>%
              left_join(EC_freqs %>%                           # Add this
                          group_by(Zone) %>% 
                          summarise(max_bar = max(Catchfreq*50, na.rm = TRUE)),
                        by = "Zone") %>%
              mutate(true_max = pmax(max_y, max_bar)) %>%      # Get true max
              group_by(Zone, Decade) %>%
              arrange(Spec_Known) %>%
              mutate(
                ymax = cumsum(prop),
                ymin = lag(ymax, default = 0),
                ymid = true_max * 1.05 + (ymin + ymax)/2 * (true_max * 0.10)  # Above everything
              ),
            aes(x = Decade, y = ymid, 
                height = true_max * 0.10 * prop,  # 10% height bar above data
                fill = Spec_Known),
            alpha = 0.7, 
            width = 8) +
  geom_col(data = EC_freqs,            
           aes(x = Decade, y = Catchfreq*50),
           color = "black",
           fill = "white",
           width = 10,
           alpha = 0.5) +
  geom_smooth(aes(x = Year, y = Lmax), 
              method = "gam",
              formula = y ~ s(x),
              color = "black", size = 0.5) +
  geom_point(aes(x = Year, y = Lmax),
             size = 1, alpha = 1,
             color = "black") +
  geom_point(data = PupTrendDat %>% 
                filter(!is.na(specpup)) %>%
                left_join(SizeTrendDat %>% 
                            group_by(Zone) %>% 
                            summarise(max_y = max(Lmax, na.rm = TRUE)), 
                          by = "Zone") %>%
                left_join(EC_freqs %>% 
                            group_by(Zone) %>% 
                            summarise(max_bar = max(Catchfreq*50, na.rm = TRUE)),
                          by = "Zone") %>%
                mutate(true_max = pmax(max_y, max_bar, na.rm = TRUE),
                       y_pos = -true_max * 0.05),  # 5% of plot height
              aes(x = Year, y = y_pos, color = specpup, 
                  size = pupup), 
              alpha = 0.75,
              shape = "*") +  #like this
  scale_size(range = c(6, 10), 
             breaks = c(1, 2),  # Only show 1 and 2
             name = "Pupping Events") +
  scale_fill_manual(values = species_colors, name = "Species") +  
  scale_color_manual(values = species_colors, name = "Species") +  
  scale_y_continuous(
    name = "Maximum TL (cm)",
    sec.axis = sec_axis(~.*1/50, name = "Count")
  ) +
  theme_clean() +
  theme(
    text = element_text(family = "optima"),
    axis.title = element_text(size = 13),
    strip.text = element_text(size = 12),
    axis.text = element_text(size = 11),
    strip.background = element_blank(),
    legend.position = "bottom",
    legend.background = element_blank(),
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0), 
    legend.box.margin = margin(t = -10, r = 0, b = 0, l = 0)
  ) +
  guides(
    color = "none",        
    fill = "none",
    size = guide_legend(title = "Pupping Events",
                        theme = theme(legend.title = element_text(family = "optima",
                                                                  size = 13))
    )
  )

ggsave(
  "fig8.png",
  plot = last_plot(),
  device = NULL,
  path = "figs/fig8",
  scale = 1,
  width = 8,
  height = 5,
  units = c("in", "cm", "mm", "px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
)
