# Details ----
#' 3_density.R
#' Paper: [Est. extent of range contraction and extinction timeline]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 2025-05-29
#' Content: regional sighting density
#' + show breakdown of confirmed/etc
#' 
#' Output: density figure (fig7.png/tiff)
#' + separate table of all data sources for appendix (incoming)
#' -----------

pacman::p_load('readxl', 'writexl', 
               'tidyverse', 'dplyr', 'tidyr',
               'sf')

EC_Date <- readxl::read_xlsx("data/xls/processed/EC_Date.xlsx") 

#Density----
library(ggridges)

#check species counts
EC_Dens <- EC_Date %>%
  filter(Year > 1850,
         Rec_Acc %in% c(1, 2, 3)) %>% 
  group_by(Region, Basin) %>%
  mutate(
    count = n(),
    Basin_plot = if_else(count <= 2, NA_character_, as.character(Basin))
  ) %>%
  ungroup() %>% 
  dplyr::select(-count) %>% 
  mutate(Region = fct_reorder(Region, mean_lat),
         Basin_plot = fct_reorder(Basin_plot, mean_lat),
         Zone = fct_reorder(Zone, mean_lat))

# Run to here on 1st Load----

##all spec----
ggplot(EC_Dens, aes(x = Year, y = Region)) +
  stat_density_ridges(
    aes(fill = Basin_plot, 
        height = after_stat(ndensity) #this one makes it relative to each row
        ),
    alpha = 0.7,
    scale = 1.25,
    bandwidth = 2,  
    show.legend = FALSE
  ) +
  geom_segment(
    data = filter(EC_Date, Rec_Acc == 3),
    aes(x = Year, xend = Year, 
        y = as.numeric(Region) - 0.15, 
        yend = as.numeric(Region) + 0.15),  # Small vertical lines
    color = "black",
    alpha = 0.7,
    linewidth = 0.5
  ) +
  geom_rug(
    data = filter(EC_Date, Rec_Acc == 3),
    aes(x = Year),
    color = "black",
    alpha = 0.7,
    sides = "b"  
  ) +
  scale_fill_viridis_d(
    name = "River Region",
    option = "turbo",
    direction = 1 
  ) +
  labs(title = "", x = "Year", y = "Drainage Basin") +
  theme_minimal() +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 12))

ggsave(
  "figs/fig7/Fig7.png",
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 9,
  height = 5,
  units = c("in", "cm", "mm", "px"),
  #dpi = 300,
  limitsize = TRUE,
  bg = NULL,
)

ggplot(EC_Dens) +
  geom_density(aes(x = Year, fill = Region),
               alpha = 0.7) +
  facet_wrap(~ fct_rev(Zone))

##Mean Year Peak----
get_mode <- function(x) {
  x <- x[!is.na(x)]  # Remove NAs
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

EC_Dens %>% 
  group_by(Zone) %>% 
  summarise(
    meanYear = mean(Year, na.rm = TRUE),
    sdYear = sd(Year, na.rm = TRUE),
    medYear = median(Year, na.rm = TRUE),
    modeYear = get_mode(Year),
    maxYear = max(Year, na.rm = TRUE)
  )

##some stats----
model <- glm(Year ~ Zone, data = EC_Dens)
summary(model) #yes the 3x northern zones are later than the 3x southern.
summary(aov(Year ~ Zone, data = EC_Dens))

# QQ plot of residuals
ggplot(data.frame(residuals = residuals(model)), aes(sample = residuals)) +
  geom_qq() +
  geom_qq_line() #not even bad!
