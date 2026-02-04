# K-medoids clustering for EERA weather years with 4-hour aggregation
# Objective: Select 5 representative + 1 extreme weather year for SDDP optimization

# Load required libraries
library(dplyr)
library(tidyr)
library(cluster)  # for pam (K-medoids)
library(readr)

# ===== CONFIGURATION =====
# Hypothetical renewable mix for net demand calculation (must sum to 1)
# This represents a high-renewable future system
REN_MIX <- list(
  pv = 0.40,
  onwind = 0.35,
  offwind = 0.25
)

# Number of hours to aggregate (4-hour blocks)
AGG_HOURS <- 4

# Number of representative years (excluding extreme year)
N_CLUSTERS <- 5

# ===== 1. LOAD DATA =====
cat("Loading CSV files...\n")

# Load all four datasets
df_pv <- read.csv("EERA_DE_PV_2028.csv", sep = ";")
df_offwind <- read.csv("EERA_DE_Offwind_2028.csv", sep = ";")
df_onwind <- read.csv("EERA_DE_Onwind_2028.csv", sep = ";")
df_demand <- read.csv("EERA_DE_Demand_2028.csv", sep = ";")

# Check data dimensions
cat(sprintf("Data loaded: %d rows\n", nrow(df_pv)))

# ===== 2. NORMALIZE DEMAND =====
cat("Normalizing demand data...\n")

# Extract weather scenario columns (WS01, WS02, etc.)
ws_cols <- grep("^WS", colnames(df_demand), value = TRUE)

# Normalize demand to [0,1] range for each weather scenario
df_demand_norm <- df_demand
for (col in ws_cols) {
  min_val <- min(df_demand[[col]], na.rm = TRUE)
  max_val <- max(df_demand[[col]], na.rm = TRUE)
  df_demand_norm[[col]] <- (df_demand[[col]] - min_val) / (max_val - min_val)
}

# ===== 3. CALCULATE NET DEMAND =====
cat("Calculating normalized net demand...\n")

# Net demand = Demand - Renewable production
# With hypothetical renewable mix: NetDemand = Demand - (a*PV + b*Onwind + c*Offwind)
df_net_demand <- df_demand_norm[, c("Date", "Hour")]

for (col in ws_cols) {
  ren_production <- REN_MIX$pv * df_pv[[col]] + 
    REN_MIX$onwind * df_onwind[[col]] + 
    REN_MIX$offwind * df_offwind[[col]]
  
  df_net_demand[[col]] <- df_demand_norm[[col]] - ren_production
}

# ===== 4. AGGREGATE TO 4-HOUR BLOCKS =====
cat(sprintf("Aggregating to %d-hour time steps...\n", AGG_HOURS))

# Create 4-hour block index
df_net_demand$block <- rep(1:ceiling(nrow(df_net_demand) / AGG_HOURS), 
                           each = AGG_HOURS, 
                           length.out = nrow(df_net_demand))

# Aggregate by taking mean over each 4-hour block
df_net_agg <- df_net_demand %>%
  group_by(block) %>%
  summarise(across(all_of(ws_cols), ~ mean(.x, na.rm = TRUE)), .groups = "drop")

cat(sprintf("Aggregated to %d time steps\n", nrow(df_net_agg)))

# ===== 5. PREPARE MATRIX FOR CLUSTERING =====
# Each column is a weather scenario, each row is a time step
net_demand_matrix <- as.matrix(df_net_agg[, ws_cols])
# Transpose so each row is a weather year (scenario)
net_demand_matrix <- t(net_demand_matrix)

cat(sprintf("Matrix for clustering: %d scenarios x %d timesteps\n", 
            nrow(net_demand_matrix), ncol(net_demand_matrix)))

# ===== 6. IDENTIFY EXTREME YEAR =====
cat("Identifying extreme (catastrophe) year...\n")

# Extreme year = highest average net demand (worst case for renewable system)
# This represents a year with low renewable production and/or high demand
avg_net_demand <- rowMeans(net_demand_matrix)
extreme_idx <- which.max(avg_net_demand)
extreme_year <- ws_cols[extreme_idx]

cat(sprintf("Extreme year identified: %s (avg net demand: %.3f)\n", 
            extreme_year, avg_net_demand[extreme_idx]))

# Remove extreme year from clustering pool
clustering_matrix <- net_demand_matrix[-extreme_idx, ]
clustering_ws <- ws_cols[-extreme_idx]

# ===== 7. K-MEDOIDS CLUSTERING =====
cat(sprintf("Running K-medoids clustering (k=%d)...\n", N_CLUSTERS))

set.seed(42)  # For reproducibility
pam_result <- pam(clustering_matrix, k = N_CLUSTERS, metric = "euclidean")

# Get medoid indices (representative years)
medoid_indices <- pam_result$medoids
representative_years <- clustering_ws[pam_result$id.med]

cat("Representative years selected:\n")
for (i in 1:N_CLUSTERS) {
  cluster_size <- sum(pam_result$clustering == i)
  cat(sprintf("  Cluster %d: %s (representing %d years)\n", 
              i, representative_years[i], cluster_size))
}

# ===== 8. CREATE FINAL SELECTION =====
final_years <- c(representative_years, extreme_year)
cat(sprintf("\nFinal selection (5 representative + 1 extreme): %s\n", 
            paste(final_years, collapse = ", ")))

# ===== 9. AGGREGATE ORIGINAL DATA TO 4-HOUR BLOCKS =====
cat("Aggregating original time series to 4-hour blocks...\n")

aggregate_to_4h <- function(df) {
  df$block <- rep(1:ceiling(nrow(df) / AGG_HOURS), 
                  each = AGG_HOURS, 
                  length.out = nrow(df))
  
  df_agg <- df %>%
    group_by(block) %>%
    summarise(across(all_of(ws_cols), ~ mean(.x, na.rm = TRUE)), .groups = "drop")
  
  return(df_agg)
}

pv_agg <- aggregate_to_4h(df_pv)
onwind_agg <- aggregate_to_4h(df_onwind)
offwind_agg <- aggregate_to_4h(df_offwind)
demand_agg <- aggregate_to_4h(df_demand)

# ===== 10. EXPORT RESULTS TO EXCEL =====
cat("Exporting results to Excel...\n")

# Load required library for Excel operations
library(openxlsx)

# Load existing Excel file
wb <- loadWorkbook("DATA_MAJOR.xlsx")

# Create time index for 4-hour blocks (2190 blocks per year for 8760 hours)
n_blocks <- nrow(pv_agg)
time_index <- 1:n_blocks

# Function to export data to specific sheet with truncation
export_to_sheet <- function(df_agg, sheet_name, selected_years, apply_truncation = TRUE) {
  # Prepare export dataframe
  df_export <- data.frame(TimeBlock = time_index)
  
  for (year in selected_years) {
    # Apply truncation threshold for capacity factors (set to 0 if < 0.01)
    if (apply_truncation) {
      df_export[[year]] <- ifelse(df_agg[[year]] > 0.01, df_agg[[year]], 0)
    } else {
      df_export[[year]] <- df_agg[[year]]
    }
  }
  
  # Write to specified sheet (overwrite existing content)
  writeData(wb, sheet = sheet_name, x = df_export, startRow = 1, startCol = 1)
  cat(sprintf("  Exported to sheet: %s\n", sheet_name))
}

# Export to respective sheets
# Demand: no truncation applied (actual demand values)
export_to_sheet(demand_agg, "LOAD", final_years, apply_truncation = FALSE)

# Capacity factors: apply 0.01 truncation threshold
export_to_sheet(pv_agg, "SOLAR", final_years, apply_truncation = TRUE)
export_to_sheet(onwind_agg, "ONWIND", final_years, apply_truncation = TRUE)
export_to_sheet(offwind_agg, "OFFWIND", final_years, apply_truncation = TRUE)

# Save the modified workbook
saveWorkbook(wb, "DATA_MAJOR.xlsx", overwrite = TRUE)
cat("  Saved: DATA_MAJOR.xlsx\n")


