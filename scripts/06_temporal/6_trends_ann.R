#when do we see regions, if ever, dip below avg sawfish capture freq. 
#need moving average of decades?
source("1_Hist_Set.R")
EC_News <- EC_Date %>% 
  dplyr::filter(Date_Acc < 3,
         Date_Acc > 0,
         Year > 1850,
         Spec_Conf == 1,
         Data_Met == "News/Media", #bc 0's are real this way, could add in museums too but
         !is.na(Basin)) 

#annual trends----
library(pracma)  

# calculate annual counts per basin (again...)
annual_counts <- EC_Date %>% 
  group_by(Region, Year) %>% 
  #summarise(ann_catch = sum(Abund, na.rm = TRUE), .groups = "drop") %>% this is totals
  summarise(ann_catch = n(), .groups = "drop") %>% #this is events
  # fill in missing years with 0, assuming they're real
  complete(Region, Year = min(EC_Date$Year):max(EC_Date$Year), 
           fill = list(ann_catch = 0))

# find peaks!
basin_stats <- annual_counts %>% 
  group_by(Region) %>% 
  summarise(n_years = n_distinct(Year[ann_catch > 0]), 
            max_catch = max(ann_catch), 
            .groups = "drop")

good_basins <- basin_stats[basin_stats$n_years >=5,  
                           "Region", drop = TRUE]

print(good_basins) #not dropping why

# Process each basin for peaks
library(tidyr)
peak_results <- data.frame()

peak_results <- annual_counts %>% 
  dplyr::filter(Region %in% good_basins) %>%
  group_by(Region) %>% 
  group_modify(~ {
    peaks <- findpeaks(.x$ann_catch, 
                       minpeakheight = 0.5, 
                       minpeakdistance = 1)
    if(!is.null(peaks) && nrow(peaks) > 0) {
      # Create the peak data
      peak_data <- tibble(
        peak_year = .x$Year[peaks[,2]],
        peak_height = peaks[,1]
      )
      # SORT BY YEAR FIRST
      peak_data %>% 
        arrange(peak_year) %>% 
        mutate(cycle_length = c(NA, diff(peak_year)))
    } else {
      tibble(peak_year = numeric(0), peak_height = numeric(0), cycle_length = numeric(0))
    }
  })

print(peak_results)

peak_summary <- peak_results %>% 
  group_by(Region) %>% 
  summarise(
    n_peaks = n(),
    avg_cycle = mean(cycle_length, na.rm = TRUE),
    med_cycle = median(cycle_length, na.rm = TRUE),
    min_cycle = min(cycle_length, na.rm = TRUE),
    max_cycle = max(cycle_length, na.rm = TRUE),
    .groups = "drop"
  )

plot_peaks <- function() {
  plot_data <- annual_counts %>% 
    dplyr::filter(Region %in% good_basins) %>% 
    left_join(peak_results %>% 
                dplyr::select(Region, peak_year, peak_height), 
              by = c("Region", "Year" = "peak_year"))
  
  ggplot(plot_data, aes(x = Year, y = ann_catch)) +
    geom_line(alpha = 0.7) +
    geom_point(size = 1) +
    geom_point(aes(y = peak_height), color = "red", size = 3, na.rm = TRUE) +
    facet_wrap(~Region, scales = "free_y") +
    labs(title = "Annual Catches with Detected Peaks (Red Points)",
         x = "Year", y = "Annual Catch") +
    theme_minimal()
}

plot_peaks()

#almost... its trying to find cycles
#whats just the avg time between reports?
time_lags <- EC_Dens %>% 
  group_by(Region) %>% 
  arrange(Year) %>%  # Sort by year within each region
  mutate(time_lag = Year - lag(Year)) %>%  # Calculate gap from previous sighting
  filter(!is.na(time_lag)) %>%  # Remove first observation (no previous sighting)
  
  mutate(
    first_continuous = which(time_lag <= 5)[1],
    # Keep only data from first continuous period onward
    continuous_data = ifelse(row_number() >= first_continuous, time_lag, NA)
  ) %>%
  
  summarise(
    n_gaps = n(),  # Number of gaps
    n_dist = n_distinct(Year),
    min_lag = min(time_lag),
    #max_lag = max(time_lag), 
    median_lag = median(time_lag),
    mean_lag = mean(time_lag),
    se_lag = sd(time_lag) / sqrt(n()),

    n_cont_gaps = sum(!is.na(continuous_data)),
    mean_cont = mean(continuous_data, na.rm = TRUE),
    se_cont = ifelse(sum(!is.na(continuous_data)) > 1, 
                           sd(continuous_data, na.rm = TRUE) / sqrt(sum(!is.na(continuous_data))), 
                           NA),
    median_cont = median(continuous_data, na.rm = TRUE),
    
    .groups = "drop"
  ) %>% arrange(mean_cont) 

print(time_lags)

individual_lags <- EC_Dens %>% 
  group_by(Region) %>% 
  arrange(Year) %>% 
  mutate(time_lag = Year - lag(Year)) %>% 
  filter(!is.na(time_lag)) %>%
  dplyr::select(Region, Year, time_lag) %>% 
  arrange(Region) 
  
kruskal.test(time_lag ~ Region, data = individual_lags)
