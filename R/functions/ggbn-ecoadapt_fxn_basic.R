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

