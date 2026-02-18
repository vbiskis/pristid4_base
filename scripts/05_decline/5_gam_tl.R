# Details ----
#' 5_gam_tl.R
#' Paper: [Est. extent of range contraction and extinction timeline]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 2025-06-03
#' Content: 
#' Input: EC_Dec.xlsx
#' Output: GAM results, sup figs, stats for results
#' -----------

source('helpers/help_stat.R')
library(purrr)
library(broom)

EC_Dens <- readxl::read_xlsx("data/xls/processed/EC_Dens.xlsx") 

# gams----
#remove juveniles and outliers:
EC_Ad <- EC_Dens %>% 
  filter(TL >= 200 & TL < 780 # or just log it?
         | Mat == 'adult') %>% 
  mutate(
    Spec_Known = factor(Spec_Known,
                        levels = c('A. cuspidata',
                                   'P. clavata',
                                   'P. pristis',
                                   'P. zijsron', 
                                   'Pristidae')),
    Zone = factor(Zone,
                  levels = c('Far North QLD',
                                    'North QLD', 'Central QLD',
                                    'Wide Bay QLD', 'South East QLD',
                                    'North Coast NSW'))) %>% 
  filter(Zone != "Mid Coast NSW" & 
           Zone != "Cape York QLD") 

lm_results <- EC_Ad %>%
  group_by(Zone) %>%
  nest() %>%
  mutate(
    model = purrr::map(data, ~lm(log(TL) ~ Year, data = .)),
    tidied = purrr::map(model, tidy),
    glanced = purrr::map(model, glance)
  ) %>%
  unnest(tidied) %>%
  filter(term == "Year")

# View
lm_results %>% dplyr::select(Zone, estimate, std.error, p.value) #non sig

# gams now!----
gam_TLyear0 <- gam(TL ~ s(Year, k = 5), data = EC_Ad)
summary(gam_TLyear0)
draw(gam_TLyear0)

#gam_TLyear1 <- gam(Lmax ~ s(Year, by = Zone, k = 5), data = SizeTrendDat)
gam_TLyear1 <- gam(TL ~ s(Year, by = Zone), data = EC_Ad) #ooh all top regions
summary(gam_TLyear1)
anova(gam_TLyear0, gam_TLyear1) #def include zone
draw(gam_TLyear1)

#gam_TLyear2 <- gam(Lmax ~ s(Year) + Zone, data = SizeTrendDat)
gam_TLyear2 <- gam(TL ~ s(Year) + Zone, data = EC_Ad)
summary(gam_TLyear2) #no
anova(gam_TLyear1, gam_TLyear2) #with zone included is much better

#gam_TLyear3 <- gam(Lmax ~ s(Year, by = Zone) + specbig, data = SizeTrendDat)
gam_TLyear3 <- gam(TL ~ s(Year, by = Zone) + Spec_Known, data = EC_Ad)
summary(gam_TLyear3)
draw(gam_TLyear3)
anova(gam_TLyear1, gam_TLyear3) #yes sig

#gam_TLyear4 <- gam(Lmax ~ s(Year) + Zone*specbig, data = SizeTrendDat)
gam_TLyear4 <- gam(TL ~ s(Year) + Zone*Spec_Known, data = EC_Ad)
summary(gam_TLyear4)
anova(gam_TLyear3, gam_TLyear4) #nooo

#gam_TLyear5 <- gam(Lmax ~ s(Year, by = specbig) + Zone, data = SizeTrendDat)
gam_TLyear5 <- gam(TL ~ s(Year, by = Spec_Known) + Zone, data = EC_Ad)
summary(gam_TLyear5)
anova(gam_TLyear3, gam_TLyear5) 

gam_TL <- gam(TL ~ Zone, data = EC_Ad)
summary(gam_TL)
anova(gam_TL, gam_TLyear2) #well damn

gam_TL1 <- gam(TL ~ Zone + Spec_Known, data = EC_Ad)
summary(gam_TL1)
anova(gam_TL, gam_TL1) #k and

gam_TL2 <- gam(TL ~ Zone*Spec_Known, data = EC_Ad)
summary(gam_TL2)
anova(gam_TL1, gam_TL2) #no #kk
anova(gam_TL1, gam_TLyear3)

# check----
performance(gam_TLyear3)
gam.check(gam_TLyear3)
appraise(gam_TLyear3) #winner!

## plot----
ggsave(
  "figs/s8/s8b_res_plot.png",
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 6,
  height = 4,
  units = c("in", "cm", "mm", "px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL
)

# plot main----
p3 <- plot_predictions(gam_TLyear3, condition = c("Year", "Zone"), 
                       vcov = FALSE,
                       rug = TRUE) +
  labs(y = "Maximum Total Length (cm)") +
  theme(legend.position = "bottom") #yiks! way outside bounds

p4 <- plot_predictions(gam_TLyear3, condition = c("Spec_Known")) +
  labs(y = "", x = "") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "italic"),)  

#fix p3----
pred_data <- EC_Ad %>%
  mutate(NS = if_else(Zone %in% c("Far North QLD", "North QLD", "Central QLD"), 
                        "N", "S")) %>% 
  group_by(NS, Zone, Spec_Known) %>%
  reframe(Year = seq(min(Year), 
                     max(Year), length.out = 200),
          Spec_Known = "Pristidae")

preds <- predictions(gam_TLyear3, newdata = pred_data)

EC_Ad <- EC_Ad %>% 
  mutate(NS = if_else(Zone %in% c("Far North QLD", "North QLD", "Central QLD"), 
                      "N", "S"))

p3 <- ggplot(preds, 
             aes(x = Year, 
                 y = estimate, 
                 color = Zone)) +
  facet_wrap( ~ NS, nrow = 2) +
  geom_line(aes(y = estimate), linewidth = 1) +
  geom_line(aes(y = conf.low), linetype = "dotted") +  
  geom_line(aes(y = conf.high), linetype = "dotted") +
  geom_rug(data = EC_Ad, aes(x = Year), sides = "b", inherit.aes = FALSE) +
  labs(y = "Mean Total Length (cm)", x = "Year") +
  theme_minimal() +
  scale_color_brewer(palette = "Paired") +
  theme(axis.title.x = element_text(margin = margin(t = 10, b = 0)),
        legend.margin = margin(l = 20),
        strip.text = element_blank()) +
  guides(color = guide_legend(nrow = 3, byrow = TRUE))


library(gtable)
library(grid)

gt <- ggplotGrob(p3)
legend <- gtable_filter(gt, "guide-box")
legend_row <- wrap_plots(legend, plot_spacer(), widths = c(3, 1))

p3_nl <- p3 + theme(legend.position = "none")

p3_nl + (p4 / legend_row + plot_layout(heights = c(8, 1))) + 
  plot_layout(widths = c(1.25, 1)) +
  plot_annotation(tag_levels = list(c('a)', 'b)'))) 

ggsave(
  "figs/s8/s8a_partialsize_plot.tiff",
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 6,
  height = 4,
  units = c("in", "cm", "mm", "px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
)



