# Details ----
#' 2_fig3_datasource.R
#' Paper: [Est. extent of range contraction and extinction timeline]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 2025-06-03
#' Content: overlay basins on east coast map
#' Dependencies: 
#'  + not much! - just xlsx of east coast
#' Output: methods data figure (fig3.png/tiff)
#' -----------

source('helpers/help_news.R')

EC_Date <- readxl::read_xlsx("data/xls/processed/EC_Date.xlsx") 
set_theme(mytheme)

#Fig 5.3 - Data Source Simple ----
##1 - Get Summary----
sourcetib <- EC_Date %>% #simplest, but doesnt show gaps in data
  group_by(Dataset) %>% 
  summarise(
    start = min(Year, na.rm = TRUE),
    end = max(Year, na.rm = TRUE)
  )

##2 - Get Data Source Gaps ----
sourcetib <- EC_Date %>%
  group_by(Data_Met) %>%
  arrange(Year) %>%
  mutate(
    year_diff = Year - lag(Year, default = first(Year)),  # Use first(Year) not Year
    new_period = year_diff > 5 | is.na(lag(Year))
  ) %>%
  mutate(
    period = cumsum(new_period)
  ) %>%
  group_by(Data_Met, period) %>%
  summarise(
    start = min(Year),
    end = max(Year),
    n_years = n(),
    .groups = "drop"
  ) %>%
  filter(n_years > 0) %>%
  mutate(
    end = ifelse(start == end, start + 0.8, end)
  )

##Plot it----
sourcecov <- ggplot(sourcetib) +
  geom_segment(aes(x = start, xend = end, 
                   y = Data_Met, yend = Data_Met, color = Data_Met), 
               size = 6) +
  scale_color_manual(
    values = c("SARA Database" = "#4682C0",
               "Public Institution" = "#2F7000",
               "News/Media" = "#8B4513"),
    guide = "none"  # Remove legend
  ) +
  scale_y_discrete(
    labels = c("trove" = "Newspapers (NLA)", "saws" = "SawSearch (SARA)",
               "museums" = "Museum Archives", "cytags" = "Cytags (SARA)")
  ) +
  scale_x_continuous(expand = expansion(add = c(10, 20))) +
  labs(x = "Year", y = "Data Source") +
  theme(plot.margin = margin(5, 5, 5, 5))

ggsave(
  "fig3.png",
  plot = sourcecov,
  device = NULL,
  path = "figs/fig3/",
  scale = 1,
  width = 6,
  height = 2,
  units = c("in", "cm", "mm", "px"),
  #dpi = 300,
  limitsize = TRUE,
  bg = NULL,
)
