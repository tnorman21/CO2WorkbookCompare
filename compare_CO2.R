# Load requirements and functions
source("requirements.R")
source("functions.R")

# --- INPUT FILE PATHS ---
inv1_path <- "~/R_Scripts/CO2ComparisonTesting/TestFiles/Boone_Start_RP_CO2_03_04_2022.xlsx"
inv2_path <- "~/R_Scripts/CO2ComparisonTesting/TestFiles/Boone_Start_RP_CO2_05_30_2025.xlsx"

# Load data
inv1 <- load_inventory(inv1_path)
inv2 <- load_inventory(inv2_path)

# Filter out saplings
inv1 <- filter_saplings(inv1, dbh_threshold = 5)
inv2 <- filter_saplings(inv2, dbh_threshold = 5)

# Match trees
matched <- match_trees(inv1, inv2)

# Find mismatches
mismatches <- find_mismatches(matched)

# Rank by impact
impact_rank <- rank_by_impact(mismatches)

# Optional: detect possible swaps
swaps <- detect_tree_id_swaps(matched)

# Save outputs
write.xlsx(list(
  Mismatches = mismatches,
  ImpactRank = impact_rank,
  PossibleSwaps = swaps
), "co2_differences.xlsx")

# Create plots
p1 <- plot_distributions(inv1, inv2, "dbh_in")
p2 <- plot_distributions(inv1, inv2, "total_height_ft")

ggsave("dbh_distribution.png", p1, width = 7, height = 5)
ggsave("height_distribution.png", p2, width = 7, height = 5)
