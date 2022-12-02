# updated: 2022-12-01 ----
# ========================================================== -----
# GGBN-EcoAdapt functions ----
# ========================================================== -----
# FUTURE CHANGE ----
# By variable ----
# Determine variable average across all scenarios  
#   Use the difference between the historic and future minimum as input 
#   Bin values and count the number of points per bin
#   Abundance is the percent of total points within each bin 
#   Study area comprised of 92785 points (= total points)
#   fxn_abundance_by_variable ----
fxn_abundance_by_variable <- function(index_data, index_variable, index_bin_size){
  
  # Subset input data to variable of interest ----
  subset <- 
    index_data %>%
    filter(variable %in% index_variable) %>%
    # Exclude NA values 
    drop_na(future_minimum_difference) %>%
    select(point_id, 
           variable_metric, 
           variable, 
           metric,
           value = future_minimum_difference)  
  
  
  # Create a helper to reshape columns ----
  n_column <- 
    lookup_variables %>%
    filter(variable %in% index_variable) %>%
    distinct(metric) %>%
    nrow() + 2
  
  # Prepare bins for grouping ----
  # Get max and min to define bins 
  value_min <- floor(min(subset$value))
  value_max <- ceiling(max(subset$value))
  
  # Create sequence with an interval of 0.025
  interval_sequence <- seq(from = value_min, to = value_max, by = index_bin_size)
  
  # Create annotation for bins
  bin_annotation <- 
    subset %>%
    # Use the sequence to bin the values for each variable 
    mutate(bin = cut(value, interval_sequence, include.lowest = TRUE)) %>%
    arrange(bin) %>%
    distinct(bin) %>%
    mutate(bin_from = word(bin, 1, sep = "\\,"), 
           bin_from = as.numeric(str_remove_all(bin_from, "\\(")), 
           bin_to = word(bin, 2, sep = "\\,"), 
           bin_to = as.numeric(str_remove_all(bin_to, "\\]")), 
           n_bin = 1:n()) 
  
  # Determine abundance (% total points) in each bin ----
  abundance <- 
    subset %>%
    # Use the sequence to bin the values for each variable 
    mutate(bin = cut(value, interval_sequence, include.lowest = TRUE)) %>%
    # Count the values for each bin by variable (for the numerator)
    group_by(variable_metric,
             variable, 
             bin) %>%
    summarize(count = n()) %>%
    ungroup() %>%
    # Create all combinations to identify missing values
    spread(variable_metric, count) %>%
    gather(variable_metric, count, 3:all_of(n_column)) %>%
    # Replace NA with 0
    mutate(count = ifelse(is.na(count), 0, count), 
           # Calculate percent by bin
           percent = count/n_points, 
           bin_size = index_bin_size) %>%
    select(-count) %>%
    spread(variable_metric, percent) %>%
    # Join annotation for bins
    left_join(bin_annotation, "bin") %>%
    relocate(n_bin, 
             bin, 
             bin_from,
             bin_to, 
             bin_size,
             variable)
}
# By variable_subset ----
# Determine abundance by scenario for each variable_metric 


# ========================================================== -----
