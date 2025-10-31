# 1. Load inventory data
load_inventory <- function(file_path, sheet_name = NULL) {
  read_excel(file_path, sheet = "TreeData") %>%
    janitor::clean_names()
}

# 2. Filter out saplings
filter_saplings <- function(df, dbh_threshold = 5) {
  df %>% filter(dbh_in >= dbh_threshold)
}

# 3. Match trees between inventories
match_trees <- function(inv1, inv2) {
  full_join(inv1, inv2, 
            by = c("plot_id", "tree_number"), 
            suffix = c("_old", "_new"))
}

# 4. Identify mismatches
find_mismatches <- function(df) {
  df %>%
    mutate(
      species_diff = as.integer(species_fia_code_old != species_fia_code_new),
      dbh_diff = abs(dbh_in_old - dbh_in_new),
      height_diff = abs(total_height_ft_old - total_height_ft_new)
    ) %>%
    filter(
      species_diff == 1 |
      dbh_diff > 0.1 |
      height_diff > 0.1
    )
}

# 5. Rank impact by DBH (proxy for CO2 impact)
rank_by_impact <- function(df) {
  df %>%
    mutate(
      impact_score = abs(dbh_in_old - dbh_in_new) + abs(total_height_ft_old - total_height_ft_new),
      impact_score = ifelse(species_diff == 1, impact_score * 2, impact_score)
      ) %>%
    arrange(desc(impact_score))
}

# 6. Detect possible Tree ID swaps

detect_tree_id_swaps <- function(df, dbh_tolerance = 0.5, species_threshold = 0.15) {
  
  swaps <- df %>%
    group_by(plot_id) %>%
    group_modify(~ {
      plot_data <- .x
      
      # All possible old vs new combinations in the plot
      combos <- expand.grid(
        old_idx = which(!is.na(plot_data$species_fia_code_old)),
        new_idx = which(!is.na(plot_data$species_fia_code_new)),
        stringsAsFactors = FALSE
      )
      
      combos <- combos %>%
        filter(old_idx != new_idx) %>%
        mutate(
          species_dist = stringdist(
            tolower(plot_data$species_fia_code_old[old_idx]),
            tolower(plot_data$species_fia_code_new[new_idx]),
            method = "jw" # Jaro-Winkler distance
          ),
          dbh_diff = abs(plot_data$dbh_in_old[old_idx] - plot_data$dbh_in_new[new_idx]),
          likely_same = (species_dist <= species_threshold) & (dbh_diff <= dbh_tolerance)
        ) %>%
        filter(likely_same)
      
      if (nrow(combos) == 0) {
        return(tibble())  # empty tibble instead of NULL or return()
      }
      
      tibble(
        plot_id = plot_data$plot_id[1],
        tree_old_id = plot_data$tree_id[combos$old_idx],
        tree_new_id = plot_data$tree_id[combos$new_idx],
        species_old = plot_data$species_fia_code_old[combos$old_idx],
        species_new = plot_data$species_fia_code_new[combos$new_idx],
        dbh_old = plot_data$dbh_in_old[combos$old_idx],
        dbh_new = plot_data$dbh_in_new[combos$new_idx],
        species_dist = combos$species_dist,
        dbh_diff = combos$dbh_diff
      )
    }) %>%
    ungroup()
  
  swaps
}

# 7. Plot comparisons
plot_distributions <- function(inv1, inv2, var) {
  bind_rows(
    inv1 %>% mutate(inventory = "Old"),
    inv2 %>% mutate(inventory = "New")
  ) %>%
    ggplot(aes(x = .data[[var]], fill = inventory)) +
    geom_density(alpha = 0.4) +
    theme_bw() +
    labs(title = paste("Distribution of", var))
}
