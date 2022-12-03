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
#   fxn_bin_by_variable ----
# index_data = bcm_change_variable
# index_variable = "tmp"
# index_bin_size = 0.025
fxn_bin_by_variable <- function(index_data, index_variable, index_bin_size){
  
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
  
  # Define sequence of bins ----
  value_min <- floor(min(subset$value))
  value_max <- ceiling(max(subset$value))
  
  # Create sequence with an interval of 0.025
  interval_sequence <- seq(from = value_min, to = value_max, by = index_bin_size)
  
  # Create annotation for the bins ----
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
           n_bin = 1:n(), 
           bin_size = index_bin_size) 
  
  # Append bins to subset -----
  append_bins <- 
    subset %>%
    # Use the sequence to bin the values for each variable 
    mutate(bin = cut(value, interval_sequence, include.lowest = TRUE))  %>%
    left_join(bin_annotation, "bin") 
  
}
#   fxn_plot_abundance_by_variable -----
fxn_plot_abundance_by_variable <- function(index_data){
  
  index_variable <- unique(index_data$variable)
  
  plot <- 
    index_data %>%
    arrange(metric) %>%
    mutate(Metric = 
             case_when(
               metric == "avg" ~ "Annual average (AVG)", 
               metric == "djf" ~ "Winter minimum (DJF)", 
               metric == "jja" ~ "Summer maximum (JJA)")
    ) %>%
    ggdensity(x = "bin_from",
              color = "Metric", 
              fill = NA, 
              size = 1,
              ylab = "Density") +
    theme_bcm_abundance_by_variable() +
    theme(legend.position = "none") + 
    geom_vline(xintercept = 0, linetype = "dotted", linewidth = 0.8, alpha = 0.5)
  
  # Add inset legend?
  
  if(index_variable %in% "tmp"){
    
    plot + 
      scale_color_manual(values = colors_metrics_3) + 
      xlab("Minimum change in temperature (C)") 
    
  }else{
    
    plot +
      scale_color_manual(values = colors_metrics_2) +
      xlab("Minimum change in precipitation (mm)")  
  }
}
#   fxn_abundance_by_variable -----
# index_data <- bin_by_variable_tmp
fxn_abundance_by_variable <- function(index_data){

  index_variable <- unique(index_data$variable)
  index_bin_size <- unique(index_data$bin_size)
  
  # Create a helper to reshape columns ----
  n_column <- 
    lookup_variables %>%
    filter(variable %in% index_variable) %>%
    distinct(metric) %>%
    nrow() + 6
  
  # Determine abundance (% total points) in each bin ----
  abundance <- 
    index_data %>%
    # Count the values for each bin by variable (for the numerator)
    group_by(variable_metric,
             variable, 
             bin, 
             bin_from, 
             bin_to,
             n_bin,
             bin_size) %>%
    summarize(count = n()) %>%
    ungroup() %>%
    # Create all combinations to identify missing values
    spread(variable_metric, count) %>%
    gather(variable_metric, count, 7:all_of(n_column)) %>%
    # Replace NA with 0
    mutate(count = ifelse(is.na(count), 0, count), 
           # Calculate percent by bin
           percent = count/n_points) %>%
    select(-count) %>%
    spread(variable_metric, percent) %>%
    relocate(n_bin, 
             bin, 
             bin_from,
             bin_to, 
             bin_size,
             variable)
}
# ---------------------------------------------------------- -----
# By variable_metric ----
#   fxn_bin_by_variable_metric ----

fxn_bin_by_variable_metric <- function(index_data, index_variable, index_bin_size){
  
  # Subset input data to variable of interest ----
  subset <- 
    index_data %>%
    filter(variable %in% index_variable) %>%
    select(-ccsm, 
           -cnrm,
           -hadg,
           -hist) %>%
    gather(column, value, ccsm_difference:hadg_difference) %>%
    mutate(scenario = str_sub(column, 1, 4), 
           calculation = "future difference") %>%
    # Exclude NA values 
    drop_na(value) %>%
    select(point_id, 
           variable_metric, 
           variable, 
           metric,
           scenario,
           calculation,
           value)  
  
  # Define sequence of bins ----
  value_min <- floor(min(subset$value))
  value_max <- ceiling(max(subset$value))
  
  # Create sequence with an interval of 0.025
  interval_sequence <- seq(from = value_min, to = value_max, by = index_bin_size)
  
  # Create annotation for the bins ----
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
           n_bin = 1:n(), 
           bin_size = index_bin_size) 
  
  # Append bins to subset -----
  append_bins <- 
    subset %>%
    # Use the sequence to bin the values for each variable 
    mutate(bin = cut(value, interval_sequence, include.lowest = TRUE))  %>%
    left_join(bin_annotation, "bin") 
  
}
# CAN THIS BE CREATED AT ONCE INSTEAD OF BY SUBSET? -----
#   fxn_plot_abundance_by_variable_metric_facet -----
# For variables with multiple metrics, to stack vertically
fxn_plot_abundance_by_variable_metric_facet <- function(index_data){
  
  index_variable <- unique(index_data$variable)
  
  lookup <- 
    lookup_labels_variable %>%
    filter(variable %in% index_variable)
  
  index_units <- unique(lookup$units)
  index_variable_label <- unique(lookup$lab_variable)
  
  xlab_title <- paste0("Change in ", 
                       str_to_lower(index_variable_label),
                       " (", 
                       index_units, 
                       ")")
  
  index_data %>%
    left_join(lookup %>%
                select(metric, lab_metric),
              "metric") %>%
    left_join(lookup_labels_scenario, "scenario") %>%
    # Use sentence case for label headers that appear in plot 
    rename(Scenario = lab_scenario,
           Metric = lab_metric) %>%
    arrange(metric, scenario) %>%
    mutate_if(is.character, as_factor) %>%
    ggdensity(x = "bin_from",
              color = "Scenario", 
              fill = NA, 
              size = 1,
              ylab = "Density", 
              xlab = xlab_title) +
    facet_wrap(~Metric, 
               ncol = 1, 
               scales = "fixed") +
    theme_bcm_abundance_by_variable_metric_stacked() +
    geom_vline(xintercept = 0, linetype = "dotted", linewidth = 0.8, alpha = 0.5) +
    scale_color_manual(values = colors_scenarios_3) 
}
#   fxn_plot_abundance_by_variable_metric -----
# To create a plot for each variable_metric 
# index_data = bin_by_variable_metric_tmp
# index_variable_metric = "tmp_avg"
# index_path = here(path_bcm, "plots")
fxn_plot_abundance_by_variable_metric <- function(index_data, index_variable_metric, index_path){
  
  index_variable <- unique(index_data$variable)
  
  lookup <- 
    lookup_labels_variable %>%
    unite(variable_metric, c(variable, metric), remove = FALSE) %>%
    filter(variable_metric %in% index_variable_metric) 
  
  index_units <- unique(lookup$units)
  index_variable_label <- unique(lookup$lab_variable)
  
  xlab_title <- paste0("Change in ", 
                       str_to_lower(index_variable_label),
                       " (", 
                       index_units, 
                       ")")
  
  plot_title <- unique(lookup$lab_metric)
  
  index_data %>%
    filter(variable_metric %in% index_variable_metric) %>%
    left_join(lookup %>%
                select(metric, lab_metric),
              "metric") %>%
    left_join(lookup_labels_scenario, "scenario") %>%
    # Use sentence case for label headers that appear in plot 
    rename(Scenario = lab_scenario,
           Metric = lab_metric) %>%
    arrange(metric, scenario) %>%
    mutate_if(is.character, as_factor) %>%
    ggdensity(x = "bin_from",
              color = "Scenario", 
              fill = NA, 
              size = 1.5, 
              title = plot_title,
              ylab = "Density", 
              xlab = xlab_title) +
    theme_bcm_abundance_by_variable_metric_stacked() +
    theme(legend.position = "none") +
    geom_vline(xintercept = 0, linetype = "dotted", linewidth = 0.8, alpha = 0.5) +
    scale_color_manual(values = colors_scenarios_3) 
  
  ggsave(here(index_path, 
              paste0("future-change_",
                     index_variable_metric, 
                     ".png")), 
         width = 9,
         height = 5, 
         units = "in",
         dpi = 300)
  
}


#   fxn_abundance_by_variable_metric ----
# ========================================================== -----
