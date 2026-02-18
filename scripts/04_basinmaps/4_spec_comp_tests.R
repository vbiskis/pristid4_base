# Details ----
#' 4_spec_comp_tests.R
#' Paper: [Est. extent of range contraction and extinction timeline]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 2025-05-29
#' Content: comparison of Pristidae with known species patterns
#' Input: EC_Dens (modified EC_Date)
#' Output: some stats for paper
#' -----------

pacman::p_load('dplyr', 'ggplot2', 'tidyr',
               'vegan', 'indicspecies', 'betapart')

EC_Dens <- readxl::read_xlsx("data/xls/processed/EC_Dens.xlsx") 
EC_Dens$decade <- floor(EC_Dens$Year / 10) * 10

#regional tests:----
NQLD <- EC_Dens %>% 
  filter(Zone == "Far North QLD" | Zone == "North QLD")

SEQ <- EC_Dens %>% 
  filter(Zone == "Wide Bay QLD" | Zone == "South East QLD")

NNSW <- EC_Dens %>% #bc it's large enough
  filter(Zone == "North Coast NSW")

# make matrix for tests----

create_commat <- function(data, time_var = "decade") {
  # Count occurrences of each species in each time period
  comm_matrix <- data %>%
    group_by(across(all_of(time_var)), Spec_Known) %>%
    summarise(count = n(), .groups = "drop") %>%
    pivot_wider(names_from = Spec_Known, 
                values_from = count, 
                values_fill = 0)
  
  # Extract time periods as row names
  time_periods <- comm_matrix[[time_var]]
  
  # Convert to data frame and remove time variable column
  comm_matrix <- as.data.frame(comm_matrix)
  comm_matrix <- comm_matrix[, -1]  # Remove first column (time variable)
  
  # Set row names
  rownames(comm_matrix) <- time_periods
  
  return(comm_matrix)
}

comm_matrix <- create_commat(EC_Dens, time_var = "Period")

# now the fun part:
# Bray-Curtis----
bray_dist <- vegdist(comm_matrix, method = "bray")
bray_matrix <- as.matrix(bray_dist)

print(round(bray_matrix, 3))

# Calculate mean dissimilarity between consecutive time periods
time_periods <- sort(unique(EC_Dens$Period))
consecutive_dissim <- numeric(length(time_periods) - 1)

for(i in 1:(length(time_periods) - 1)) {
  consecutive_dissim[i] <- bray_matrix[i, i+1]
}

dissim_df <- data.frame(
  transition = paste(time_periods[-length(time_periods)], 
                     time_periods[-1], sep = " → "),
  dissimilarity = consecutive_dissim
)

print(dissim_df)

# Plot dissimilarity over time
ggplot(dissim_df, aes(x = transition, y = dissimilarity)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_hline(yintercept = mean(consecutive_dissim), 
             linetype = "dashed", color = "red") +
  labs(title = "Species Composition Change Between Decades",
       subtitle = "Bray-Curtis Dissimilarity",
       x = "Decade Transition",
       y = "Dissimilarity (0 = identical, 1 = completely different)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

table(EC_Dens$Period)
cor.test(1:length(consecutive_dissim), consecutive_dissim) #yes it is!

# PERMANOVA----

reg_matrix <- EC_Dens %>%
  dplyr::select(ID, Spec_Known, Period) %>%  # we need all entries as lines, not just aggregated
  mutate(present = 1) %>%
  pivot_wider(names_from = Spec_Known, 
              values_from = present, 
              values_fill = 0)

# get metadata
metadata <- reg_matrix %>% dplyr::select(ID, Period)
comm_matrix_ind <- reg_matrix %>% dplyr::select(-ID, -Period)
time_groups <- data.frame(Period = as.factor(rownames(comm_matrix_ind)))

## Now run PERMANOVA with replication----
set.seed(123)  # For reproducibility
permanova_result <- adonis2(comm_matrix_ind ~ Period, 
                            data = metadata,
                            method = "bray",
                            permutations = 999)

print(permanova_result)

cat(sprintf("- R² = %.3f: Time explains %.1f%% of compositional variation\n", 
            permanova_result$R2[1], permanova_result$R2[1] * 100))
cat(sprintf("- p-value = %.4f ", permanova_result$`Pr(>F)`[1]))

## Pairwise PERMANOVA between consecutive decades----
pairwise_results <- list()

# Get unique decades from metadata
Period <- unique(metadata$Period)
Period <- sort(Period)

for(i in 1:(length(Period) - 1)) {
  d1 <- Period[i]
  d2 <- Period[i+1]
  
  # Subset data for these two decades
  subset_indices <- which(metadata$Period %in% c(d1, d2))
  subset_comm <- comm_matrix_ind[subset_indices, ]
  subset_meta <- metadata[subset_indices, ]
  
  set.seed(123)
  pair_result <- adonis2(subset_comm ~ Period,
                         data = subset_meta,
                         method = "bray",
                         permutations = 999)
  
  pairwise_results[[paste(d1, d2, sep = "_vs_")]] <- pair_result
  
  cat(sprintf("\n%s vs %s:\n", d1, d2))
  cat(sprintf("  R² = %.3f, p = %.4f\n", 
              pair_result$R2[1], 
              pair_result$`Pr(>F)`[1]))
}

# Indicator species analysis----
indicator_result <- multipatt(comm_matrix_ind, 
                              time_groups$Period,
                              control = how(nperm = 99))

summary(indicator_result, indvalcomp = TRUE)

## Extract significant indicator species----
sig_indicators <- indicator_result$sign %>%
  filter(p.value < 0.05) %>%
  arrange(p.value)

if(nrow(sig_indicators) > 0) {
  cat("\n\nSignificant Indicator Species (p < 0.05):\n")
  sig_indicators$species <- rownames(sig_indicators)
  sig_indicators <- sig_indicators %>%
    select(species, s.1:last_col(), stat, p.value)
  print(sig_indicators)
  
  # Plot indicator values for significant species
  indicator_plot_data <- sig_indicators %>%
    head(10) %>%  # Top 10 most significant
    mutate(decade_indicated = colnames(sig_indicators)[2:(ncol(time_groups)+1)][apply(.[, 2:(ncol(time_groups)+1)], 1, which.max)])
  
  ggplot(indicator_plot_data, aes(x = reorder(species, stat), y = stat, fill = p.value)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    scale_fill_gradient(low = "darkred", high = "lightpink") +
    labs(title = "Top Indicator Species",
         x = "Species",
         y = "Indicator Value",
         fill = "p-value") +
    theme_minimal()
  
  ggsave("/mnt/user-data/outputs/indicator_species_plot.png", 
         width = 10, height = 6, dpi = 300)
  
  cat("\nPlot saved: indicator_species_plot.png\n")
} else {
  cat("\nNo significant indicator species found at p < 0.05\n")
}

# Last one: betapart----
comm_binary <- ifelse(comm_matrix > 0, 1, 0) # Convert to presence/absence 

## Calculate beta diversity----
beta_div <- beta.pair(comm_binary, index.family = "sorensen")

# Extract components
beta_sim <- as.matrix(beta_div$beta.sim)  # Turnover
beta_sne <- as.matrix(beta_div$beta.sne)  # Nestedness
beta_sor <- as.matrix(beta_div$beta.sor)  # Total beta diversity

## Calculate turnover----
consecutive_turnover <- numeric(length(time_periods) - 1)
consecutive_nestedness <- numeric(length(time_periods) - 1)
consecutive_total <- numeric(length(time_periods) - 1)

for(i in 1:(length(time_periods) - 1)) {
  consecutive_turnover[i] <- beta_sim[i, i+1]
  consecutive_nestedness[i] <- beta_sne[i, i+1]
  consecutive_total[i] <- beta_sor[i, i+1]
}

## Make df----
beta_df <- data.frame(
  transition = paste(time_periods[-length(time_periods)], 
                     time_periods[-1], sep = " → "),
  turnover = consecutive_turnover,
  nestedness = consecutive_nestedness,
  total_beta = consecutive_total,
  pct_turnover = ifelse(consecutive_total > 0, (consecutive_turnover / consecutive_total) * 100, 0),
  pct_nestedness = ifelse(consecutive_total > 0, (consecutive_nestedness / consecutive_total) * 100, 0)
)

print(beta_df) #check it

## Visualize turnover vs nestedness----
beta_long <- beta_df %>%
  dplyr::select(transition, turnover, nestedness) %>%
  pivot_longer(cols = c(turnover, nestedness), 
               names_to = "component", 
               values_to = "value")

ggplot(beta_long, aes(x = transition, y = value, fill = component)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("turnover" = "#E69F00", "nestedness" = "#56B4E9"),
                    labels = c("Nestedness (loss/gain)", "Turnover (replacement)")) +
  labs(title = "Beta Diversity Partitioning",
       subtitle = "Turnover = species replacement | Nestedness = species loss/gain",
       x = "Decade Transition",
       y = "Beta Diversity Component",
       fill = "Component") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Interpretation
avg_pct_turnover <- mean(beta_df$pct_turnover)
avg_pct_nestedness <- mean(beta_df$pct_nestedness)

cat(sprintf("- Turnover: %.1f%%\n", avg_pct_turnover))
cat(sprintf("- Nestedness: %.1f%%\n", avg_pct_nestedness))

if(avg_pct_turnover > avg_pct_nestedness) {
  cat("\nConclusion: Species REPLACEMENT (turnover) is the dominant driver of compositional change.\n")
} else {
  cat("\nConclusion: Species LOSS/GAIN (nestedness) is the dominant driver of compositional change.\n")
}


# 1. PERMANOVA stats
permanova_result$R2[1]          # R² value
permanova_result$`Pr(>F)`[1]    # p-value

# 2. Bray-Curtis stats
mean(consecutive_dissim)         # mean dissimilarity
range(consecutive_dissim)        # min and max

# 3. Beta diversity stats
mean(beta_df$pct_turnover)      # % turnover
mean(beta_df$pct_nestedness)    # % nestedness
