# ---------------------------------------------------------- -----
# Statistical tests ----
#   fxn_aov_me ----
fxn_aov_me <- function(df){
  df %>%
    anova_test(dv = value, 
               wid = plot_id, 
               within = year) %>%
    get_anova_table() %>%
    adjust_pvalue(method = "bonferroni") %>%
    as_tibble() %>%
    clean_names() %>%
    mutate(method = "me", 
           data_type = index_type) %>%
    rename(statistic = f) %>% 
    relocate(data_type, 
             method, 
             effect, 
             starts_with("p_adj"), 
             statistic,
             starts_with("d"), 
             ges)
}

#   fxn_pwc ----
# Excludes fuel_class, is for pwc of fuel_type only
fxn_pwc <- function(df){
  df %>%
    pairwise_t_test(
      value ~ year, 
      paired = TRUE,
      p.adjust.method = "bonferroni") %>%
    clean_names() %>%
    mutate(method = "pwc", 
           data_type = index_type, 
           statistic = fxn_digit(statistic)) %>%
    rename(p_adj_sig = p_adj_signif) %>%
    select(data_type, 
           method, 
           starts_with("group"),
           starts_with("p_adj"), 
           statistic,
           starts_with("d"),  
           starts_with("p"))
}

#   fxn_aov2_me ----
fxn_aov2_me <- function(df){
  df %>%
    anova_test(dv = value, 
               wid = plot_id, 
               within = c(fuel_class, year)) %>%
    get_anova_table() %>%
    adjust_pvalue(method = "bonferroni") %>%
    as_tibble() %>%
    clean_names() %>%
    mutate(method = "aov2", 
           data_type = index_type)  %>%
    rename(statistic = f) %>%
    select(data_type, 
           method, 
           effect, 
           starts_with("p_adj"), 
           statistic,
           starts_with("d"), 
           ges)
}

#   fxn_pwc2 ----
# Includes a column for fuel_class
fxn_pwc2 <- function(df){
  df %>%
    pairwise_t_test(
      value ~ year, 
      paired = TRUE,
      p.adjust.method = "bonferroni") %>%
    clean_names() %>%
    mutate(method = "pwc", 
           data_type = index_type, 
           statistic = fxn_digit(statistic)) %>%
    rename(p_adj_sig = p_adj_signif) %>%
    select(data_type, 
           method, 
           fuel_class,
           starts_with("group"),
           starts_with("p_adj"), 
           statistic,
           starts_with("d"))
  
  
}

#   fxn_tidy_model ----
fxn_tidy_model <- function(df){
  df %>%
    rownames_to_column("model") %>%
    as_tibble() %>%
    clean_names() %>%
    rename(aicc = ai_cc) %>%
    mutate(ord = 1:n(), 
           weight = as.numeric(weight))
}
# ---------------------------------------------------------- -----
# WPI file management  ----
#   img_file_name_fxn: Create image file name from deployment_id  ----
img_file_name_fxn <- function(image, deployment) {
  parenthesis_start <- str_split_fixed(image, ("\\("), 2) 
  parenthesis_end <- str_split_fixed(parenthesis_start[,2], ("\\)"), 2) 
  pad <- str_pad(parenthesis_end[,1], 5, side = "left", pad = "0") 
  pad_id <- paste("IMG-", pad, sep = "")
  pad_id_jpg <- paste(pad_id, ".JPG", sep = "")
  image_file <- paste(deployment, pad_id_jpg, sep = "_")
}
#   fxn_get_deployment_log ----
path_log <- here("/Users/mgray/Dropbox/0_data/wpi/logs/wpi-deployment-log.xlsx")
fxn_get_deployment_log <- function(index_path){
  
  # Define is.Date function ----
  is.Date <- function(x) {
    inherits(x, c("Date", "POSIXt"))}
  
  # Read xlsx and define column types ----
  
  log_preview <- 
    read_excel(here(index_path), 
               sheet = "deployments") 
  log_names <- colnames(log_preview)
  log_names_date <- log_names %>% 
    as_tibble() %>%
    filter(substr(value, 1, 4) == "date") %>%
    pull(value)
  log_names_n <- log_names %>% 
    as_tibble() %>%
    filter(substr(value, 1, 2) == "n_") %>%
    pull(value)
  log_column_data_types <- vector()
  
  for(i in 1 : length(log_names)){  
    
    #where date_ is in a column name
    if(log_names[i] %in% log_names_date){
      log_column_data_types[i] <- "date"
      
      #where n_ is in a column name
    } else if (log_names[i] %in% log_names_n) {
      log_column_data_types[i] <- "numeric"
      
    } else{
      log_column_data_types[i] <- "text"
    }
  }
  
  # Reread xlsx file and assign column types ----
  tbl_deployment <- 
    read_excel(here(index_path), 
               sheet = "deployments",  
               col_types = log_column_data_types) %>%
    mutate(across(where(is.Date), as_date)) %>%
    select(id:comments, 
           is_gap) 
  
  
}
# ---------------------------------------------------------- -----
# LTNC WPI file management -----

#   fxn_combine_xlsx ----
fxn_combine_xlsx <- function(index_xlsx){
  
  input_path <- here(path_orig, index_xlsx)
  id <- str_sub(index_xlsx, 1, 11)
  
  data <- 
    input_path %>%
    excel_sheets() %>%
    set_names() %>%
    map_df(~ read_excel(path = input_path, 
                        sheet = .x), 
           .id = "orig_sheet") %>%
    clean_names() %>%
    remove_empty("cols") %>%
    mutate(id = id) %>%
    relocate(id) 
}

#   fxn_subset_column_names -----
fxn_subset_column_names <- function(df){
  
  crosswalk <-
    tibble(names_input = names(df)) %>%
    left_join(lookup_column_names, "names_input") %>%
    filter(include == TRUE) %>%
    arrange(ord_name)
  
  orig_names <- crosswalk$names_input  
  
  names_subset <- crosswalk %>%
    # Check for duplicated column names  
    group_by(column_rename) %>%
    mutate(n = 1:n()) %>%
    ungroup() %>%
    # Create index to catch dupe names
    mutate(column_rename_n = 
             ifelse(n == 1, column_rename,
                    paste(column_rename, 
                          n,  sep = "_"))) %>%
    arrange(ord_name) %>%
    pull(column_rename_n)
  
  df %>%
    select(any_of(orig_names)) %>%
    rename_all(~ names_subset) 
  
}


#   fxn_catalog_by -----
fxn_catalog_by <- function(df){
  n_fn <- df %>%
    select(starts_with("cat_by_fn")) %>%
    ncol()
  
  n_ln <- df %>%
    select(starts_with("cat_by_ln")) %>%
    ncol()
  
  df %>%
    relocate(starts_with("cat_by_fn")) %>%
    unite(cat_by_fn,
          1:all_of(n_fn), 
          na.rm = TRUE, 
          remove = TRUE) %>%
    relocate(starts_with("cat_by_ln")) %>%
    unite(cat_by_ln, 
          1:all_of(n_ln), 
          na.rm = TRUE, 
          remove = TRUE) %>%
    relocate(cat_by_fn, .before = cat_by_ln) %>%
    unite(cat_by, 
          cat_by_fn:cat_by_ln, 
          sep = " ", 
          na.rm = TRUE, 
          remove = TRUE) 
  
}
#   fxn_qc_by -----
fxn_qc_by <- function(df){
  
  n_qc <- df %>%
    select(starts_with("qc")) %>%
    ncol()
  
  df %>%
    relocate(starts_with("qc")) %>%
    unite(qc_by,
          1:all_of(n_qc), 
          na.rm = TRUE, 
          remove = TRUE)
}

#   fxn_image_file -----
# Add dash (-) after IMG when missing in image_file value
# Create image_n

fxn_image_file <- function(df){
  
  df %>% 
    mutate(img_has_dash = str_detect(image_file, "\\IMG-")) %>%
    mutate(image_file = 
             case_when(img_has_dash == TRUE ~ 
                         image_file, 
                       img_has_dash == FALSE ~ 
                         str_replace(image_file, "IMG", "IMG-")), 
           image_n = as.numeric(str_sub(image_file, 17, 21))) %>%
    select(-img_has_dash)
  
}

#   fxn_tidy_binomial_y4 ----
# Remove double spaces 
fxn_tidy_binomial <- function(df){
  
  df %>%
    left_join(lookup_binomial, "binomial") %>%
    rename(orig_binomial = binomial, 
           orig_genus = genus, 
           orig_species = species) 
}

#   fxn_annotate ----
fxn_annotate <- function(df){
  
  df %>%
    mutate(grid = str_sub(id, 1, 1), 
           camera = str_sub(id, 3, 4), 
           year = year(date_time),
           date = as_date(date_time),
           time = hms::as_hms(date_time), 
           error_dt = ifelse(is.na(date_time), 
                             "date_time is NA", 
                             NA)) 
  
}


#   tidy_images ----
tidy_images <- 
  images_raw %>%
  fxn_subset_column_names() %>%
  fxn_qc_by() %>%
  fxn_catalog_by() %>%
  fxn_image_file() %>%
  fxn_tidy_binomial() %>%
  fxn_annotate() %>%
  select(any_of(list_column_names_final))
# ---------------------------------------------------------- -----
# Munge ----
#   %nin% ----
"%nin%" <- Negate("%in%")
#   spread_n ----
spread_n <- function(df, key, value) {
  # quote key
  keyq <- rlang::enquo(key)
  # break value vector into quotes
  valueq <- rlang::enquo(value)
  s <- rlang::quos(!!valueq)
  df %>% gather(variable, value, !!!s) %>%
    unite(temp, !!keyq, variable) %>%
    spread(temp, value)
}
#   is.Date ----
is.Date <- function(x) {
  inherits(x, c("Date", "POSIXt"))}
#   substrRight: Subset character string from the right ----
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
# ---------------------------------------------------------- -----
# Numbers ----
#   fxn_digit ----
fxn_digit <- function(x){
  as.numeric(format(round(x, 3), nsmall = 3))
}
#   fxn_signif_adj ----
fxn_signif_adj <- function(df){
  
  df %>%
    mutate(p_adj_sig = case_when(
      p_adj < 0.001 ~ "***", 
      p_adj > 0.001 & p_adj < 0.01 ~ "**", 
      p_adj > 0.01 & p_adj < 0.05 ~ "*", 
      p_adj > 0.05 ~ "n.s.")) %>%
    arrange(p_adj)
  
}
#   fxn_signif ----
fxn_signif <- function(df){
  
  df %>%
    mutate(p_sig = case_when(
      p_value < 0.001 ~ "***", 
      p_value > 0.001 & p_value < 0.01 ~ "**", 
      p_value > 0.01 & p_value < 0.05 ~ "*", 
      p_value > 0.05 ~ "n.s.")) %>%
    arrange(p_value)
  
}
#   fxn_ci95 ----
fxn_ci95 <- function(df, index_value){
  df %>%
    rename(value = all_of(index_value)) %>%
    mutate(mean = mean(value, na.rm = TRUE), 
           sd = sd(value, na.rm = TRUE), 
           count = n(), 
           margin = (qt(0.975, df = count - 1))*(sd/sqrt(count)), 
           lci = mean - margin, 
           uci = mean + margin) %>%
    select(-margin, -sd, -count)
}
#   round_any ----
round_any = function(x, accuracy, f=round){f(x/ accuracy) * accuracy}

#   fxn_transform ----
fxn_transform <- function(df){
  df %>%
    mutate(value_log = log(value + 1e-6), 
           value_std = as.vector(scale(value)),
           value_sqrt = sqrt(value))
}

# ---------------------------------------------------------- -----
# Other ----
#   fxn_kable ----
fxn_kable <- function(df){
  df  %>%
    knitr::kable() %>%
    kable_styling(bootstrap_options = c("striped", "hover"), 
                  full_width = F,  
                  position = "left", 
                  fixed_thead = T)
}

#   fxn_vegan ----
fxn_vegan <- function(index_df){
  
  data <- get(index_df)
  
  simpson <- 
    enframe(vegan::diversity(data, index = "simpson")) %>%
    rename(index = name) %>%
    mutate(metric = "simp")
  
  shannon <- 
    enframe(vegan::diversity(data, index = "shannon")) %>%
    rename(index = name) %>%
    mutate(metric = "shan")
  
  richness <- 
    enframe(vegan::specnumber(data)) %>%
    rename(index = name) %>%
    mutate(metric = "rich")
  
  bind <- 
    bind_rows(simpson, shannon, richness) %>% 
    mutate(subset = str_remove_all(index_df, "df_")) %>%
    spread(metric, value)
  
}
