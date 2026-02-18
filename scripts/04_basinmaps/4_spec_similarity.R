# Details ----
#' 4_spec_similarity.R
#' Paper: [Est. extent of range contraction and extinction timeline]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 2025-05-29
#' Content: comparison of Pristidae with known species patterns
#' Input: EC_Dens (modified EC_Date)
#' Output: N/A - just playing 
#' -----------

EC_Dens <- readxl::read_xlsx("data/xls/processed/EC_Dens.xlsx") 
EC_Dens <- EC_Dens %>% 
  filter(!(Rec_Acc == 3 & Type_S == "Specimen" & Medium == "Private collection"))

library(corrplot)
library(pheatmap)

#Table output:----
species_metrics <- EC_Spec %>% 
  group_by(Spec_Known) %>% 
  summarise(
    # Central tendency
    mean_lat = mean(Latitude, na.rm = TRUE),
    mean_lon = mean(Longitude, na.rm = TRUE),
    
    # Range/dispersion
    range_distance_km = mean(distHaversine(
      cbind(Longitude, Latitude),
      cbind(mean(Longitude), mean(Latitude))
    ) / 1000, na.rm = TRUE),
    
    #N extent
    max_lat = max(Latitude, na.rm = TRUE),
    min_lon = mean(Longitude, na.rm = TRUE),
    
    #S extent
    min_lat = min(Latitude, na.rm = TRUE),
    max_lon = mean(Longitude, na.rm = TRUE),
    
    # Frequency
    total_reports = n(),
    n_basins = n_distinct(Basin, na.rm = TRUE),
    reports_per_basin = total_reports / n_basins,
  )

#Dissimilarity----
species_scaled <- species_metrics %>% 
  column_to_rownames("Spec_Known") %>% 
  scale() %>% 
  as.data.frame()

# Euclidean distance matrix (dissimilarity)
dist_matrix <- dist(species_scaled, method = "euclidean")

# Convert to similarity (higher = more similar)
max_dist <- max(dist_matrix)
similarity_matrix <- as.matrix(max_dist - dist_matrix)
diag(similarity_matrix) <- max_dist  # Self-similarity = maximum

# Or use correlation-based similarity
similarity_matrix <- cor(t(species_scaled))

# Correlation plot
corrplot(similarity_matrix, method = "color", type = "upper", 
         order = "hclust", tl.cex = 0.8, tl.col = "black")

# Heatmap with clustering
pheatmap(similarity_matrix, 
         clustering_distance_rows = "euclidean",
         clustering_distance_cols = "euclidean")

# Or dendrogram
plot(hclust(dist_matrix), main = "Species Similarity Dendrogram")
#thats what I thought!
