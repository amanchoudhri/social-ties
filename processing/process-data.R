library(tidyr)
library(dplyr)
library(forcats)
library(haven)
library(ggplot2)
library(stringr)
library(here)

setwd(here::here())



# ----- LOAD DATA -----

# SAY 24 WAVES 6 & 10
waves <- c(6,10)
raw_data_filenames <- c()
for (wave in waves) {raw_data_filenames <- c(raw_data_filenames, paste0('dat/wave', wave, '/raw_wave_', wave, '.rds'))}
dfs <- lapply(raw_data_filenames, readRDS)
df1 <- dfs[[1]]
df2 <- dfs[[2]]

# ZIP CODES
# Data source: 
# URL: https://simplemaps.com/data/us-zips
zip_to_county_raw <- read.csv('dat/geographic/uszips.csv')
hardcoded_zips <- readRDS('dat/geographic/hardcoded_zips.rds')

# STATE-LEVEL LEANINGS
# Data source: The Cook Political Report
# URL: https://cookpolitical.com/ratings/presidential-race-ratings
# Note: Data manually transcribed on September 10, 2024
cook_ratings <- read.csv("dat/geographic/cook_political_report_ratings.csv")



# ----- DEVELOP CLEANING/PROCESSING FUNCTIONS -----


# Utility Functions -----

print_shape <- function(df) {
  cat("Rows:", nrow(df), "| Columns:", ncol(df), "\n")
  df}

print_count <- function(df, col) {
  df %>% count(!!sym(col)) %>% print()
  df
}


# Zip processing ----- 

zip_to_char <- function(zip) {
  zip_char <- as.character(zip)
  ifelse(
    nchar(zip_char) == 5,
    zip_char,
    str_pad(zip_char, 5, pad = "0", side = "left"))}

clean_zips <- function(zips) {
  zips %>%
    mutate(state_lower = tolower(state_name), 
           zipcode = zip_to_char(zip), 
           county=county_name) %>%
    left_join(hardcoded_zips, by = c("state_lower"="state", "zipcode"="zipcode")) %>%
    mutate(county = coalesce(county.y, county.x)) %>%
    select(state_lower, zipcode, county)}

add_counties <- function(df) {
  df %>%
    mutate(
      zipcode = zip_to_char(zipcode),
      state_lower = tolower(inputstate)) %>%
    left_join(zip_to_county %>% dplyr::select(zipcode, state_lower, county), 
            by = c('state_lower' = 'state_lower', 'zipcode' = 'zipcode'))
}


# PID Processing -----

friend_group_partisan_levels <- c(
  "All Democrats",
  "Mostly Democrats",
  "About evenly split",
  "Mostly Republicans",
  "All Republicans")

copartisanship_levels <- c(
  "All co-partisan",
  "Mostly co-partisan",
  "About evenly split",
  "Mostly counter-partisan",
  "All counter-partisan")

dem_levels <- friend_group_partisan_levels
rep_levels <- rev(friend_group_partisan_levels)

dem_map <- setNames(copartisanship_levels, dem_levels)
rep_map <- setNames(copartisanship_levels, rep_levels)

recode_pid <- function(df) {
  df %>%
    mutate(
      pid3 = recode(pid3, "Not sure" = "Independent", "Other" = "Independent") %>% fct_relevel(
        "Democrat", 
        "Independent", 
        "Republican"),
      pid3_baseline = recode(pid3_baseline, "Not sure" = "Independent", "Other" = "Independent") %>% fct_relevel(
        "Democrat", 
        "Independent", 
        "Republican"),
      pid7 = recode(pid7, "Not sure" = "Independent") %>% fct_relevel(
        "Strong Democrat", 
        "Not very strong Democrat", 
        "Lean Democrat", 
        "Independent",
        "Lean Republican",
        "Not very strong Republican",
        "Strong Republican"),
      pid7_baseline = recode(pid7_baseline, "Not sure" = "Independent") %>% fct_relevel(
        "Strong Democrat", 
        "Not very strong Democrat", 
        "Lean Democrat", 
        "Independent",
        "Lean Republican",
        "Not very strong Republican",
        "Strong Republican"),
      friend_group_pid3 = recode(friend_group_pid3, "Not sure" = "About evenly split") %>% fct_relevel(
        "Mostly Democrats", 
        "About evenly split", 
        "Mostly Republicans"))}

recode_friend_group_pid <- function(df) {
  df %>%
    mutate(
      # friend_group_class = friend_group_class %>% 
      #   fct_recode("Friends much wealthier" = "Much wealthier",
      #              "Friends slightly wealthier" = "Slightly wealthier",
      #              "Friends the same" = "The same",
      #              "Friends slightly poorer" = "Slightly poorer",
      #              "Friends much poorer" = "Much poorer"),
      friend_group_pid5 = case_when(
        friend_group_pid3 == "Mostly Democrats" & any_friends_republicans == "No" ~ "All Democrats",
        friend_group_pid3 == "Mostly Republicans" & any_friends_democrats == "No" ~ "All Republicans",
        TRUE ~ friend_group_pid3) %>% fct_relevel(friend_group_partisan_levels),
      friend_group_copartisanship = case_when(
        collapsed_pid == "Democrat" ~ dem_map[as.character(friend_group_pid5)],
        collapsed_pid == "Republican" ~ rep_map[as.character(friend_group_pid5)],
        TRUE ~ collapsed_pid) %>% fct_relevel(copartisanship_levels))}

use_consider_candidate <- function(df, col, dem_label, rep_label, middle_cat, dem_output="Democrat", rep_output="Republican") {
  consider_dem_col <- paste0("consider_", strsplit(tolower(dem_label), " ")[[1]][2])
  consider_rep_col <- paste0("consider_", strsplit(tolower(rep_label), " ")[[1]][2])
  consider_dem <- paste0("I would consider voting for ", dem_label)
  consider_rep <- paste0("I would consider voting for ", rep_label)
  fct_levels <- levels(df[[col]])
  df %>% 
    mutate(!!sym(col) := case_when(
      !!sym(col) == middle_cat & !!sym(consider_dem_col) == consider_dem & !!sym(consider_rep_col) == consider_rep ~ middle_cat,
      !!sym(col) == middle_cat & !!sym(consider_dem_col) == consider_dem ~ dem_output,
      !!sym(col) == middle_cat & !!sym(consider_rep_col) == consider_rep ~ rep_output,
      TRUE ~ !!sym(col)) %>% fct_relevel(fct_levels))}

collapse <- function(df, col, new_col, threshold=4, middle_cat="Independent") {
  df %>% 
    mutate(!!sym(new_col) := case_when(
      as.numeric(!!sym(col)) < threshold ~ "Democrat",
      as.numeric(!!sym(col)) > threshold ~ "Republican",
      TRUE ~ middle_cat) %>% fct_relevel("Democrat", middle_cat, "Republican"))}

make_pid_numeric <- function(df) {
  df %>%
    mutate(
      collapsed_pid_numeric = as.numeric(collapsed_pid) - 2,
      collapsed_pid_baseline_numeric = as.numeric(collapsed_pid_baseline) - 2,
      friend_group_pid_numeric = (as.numeric(friend_group_pid5) - 3)/2,
      friend_group_copartisanship_numeric = (as.numeric(friend_group_copartisanship) - 3)/2)}

process_wave_6_pid <- function(df) {
  df %>%
    recode_pid() %>%
    print_count("pid7") %>%
    use_consider_candidate("pid7", "Joe Biden", "Donald Trump", "Independent", "Lean Democrat", "Lean Republican") %>%
    print_count("pid7") %>%
    collapse("pid7", "collapsed_pid", 4) %>%
    collapse("pid7_baseline", "collapsed_pid_baseline", 4) %>%
    recode_friend_group_pid() %>%
    make_pid_numeric()
}


process_wave_10_pid <- function(df) {
  df %>%
    recode_pid() %>%
    print_count("pid7") %>%
    use_consider_candidate("pid7", "Kamala Harris", "Donald Trump", "Independent", "Lean Democrat", "Lean Republican") %>%
    print_count("pid7") %>%
    collapse("pid7", "collapsed_pid", 4) %>%
    collapse("pid7_baseline", "collapsed_pid_baseline", 4) %>%
    recode_friend_group_pid() %>%
    make_pid_numeric()
}


# Presvote Processing -----

recode_presvote <- function(df, vote_col, dem_label, rep_label, middle_cat) {
  df %>% 
    mutate(!!sym(vote_col) := case_when(
      !!sym(vote_col) == dem_label ~ "Democrat",
      !!sym(vote_col) == rep_label ~ "Republican",
      TRUE ~ middle_cat
    ) %>% fct_relevel("Democrat", middle_cat, "Republican"))}

make_presvote_wave_6_numeric <- function(df) {
  df %>%
    mutate(
      presvote24_numeric = as.numeric(presvote24) - 2,
      friend_group_presvote24_numeric = as.numeric(friend_group_presvote24) - 2,
      best_friend_presvote24_numeric = as.numeric(best_friend_presvote24) - 2)}

make_presvote_wave_10_numeric <- function(df) {
  df %>%
    mutate(
      presvote24h_numeric = as.numeric(presvote24h) - 2,
      friend_group_presvote24h_numeric = as.numeric(friend_group_presvote24h) - 2,
      presvote24post_numeric = as.numeric(presvote24post) - 2)}

process_wave_6_presvote <- function(df) {
  df %>%
    recode_presvote("presvote24", "Joe Biden", "Donald Trump", "Other") %>%
    recode_presvote("friend_group_presvote24", "Mostly Joe Biden", "Mostly Donald Trump", "About evenly split") %>%
    recode_presvote("best_friend_presvote24", "Joe Biden", "Donald Trump", "Other") %>%
    print_count("presvote24") %>%
    use_consider_candidate("presvote24", "Joe Biden", "Donald Trump", "Other") %>%
    print_count("presvote24") %>%
    make_presvote_wave_6_numeric()}

process_wave_10_presvote <- function(df) {
  df %>%
    recode_presvote("presvote24h", "Kamala Harris", "Donald Trump", "Other") %>%
    recode_presvote("friend_group_presvote24h", "Mostly Kamala Harris", "Mostly Donald Trump", "About evenly split") %>%
    recode_presvote("presvote24post", "Kamala Harris", "Donald Trump", "Other") %>%
    print_count("presvote24h") %>%
    use_consider_candidate("presvote24h", "Kamala Harris", "Donald Trump", "Other") %>%
    print_count("presvote24h") %>%
    make_presvote_wave_10_numeric()}

# Survey Cleaning Functions -----

clean_wave_6 <- function(df) {
  df %>%
    process_wave_6_pid() %>%
    rename(urbanicity = urbancity) %>% # Rename column urbancity to urbanicity
    replace_na(list(urbanicity = "City")) %>% # There is 1 NA in urbancity with an urban zipcode: population 37,974 urban vs 6,230 rural (https://www.city-data.com/zips/46360.html)
    mutate(urbanicity = recode(urbanicity, "Other" = NA)) %>%  # Change "Other" to NA for plotting functions to ignore the ~8 "Other" responses
    add_counties() %>%
    print_shape() %>%
    drop_na(presvote24, friend_group_presvote24) %>%
    print_shape() %>%
    process_wave_6_presvote()
  }

clean_wave_10 <- function(df) {
  df %>%
    process_wave_10_pid() %>%
    #rename(urbanicity = urbancity) %>%
    add_counties() %>%
    print_shape() %>%
    drop_na(presvote24h, friend_group_presvote24h) %>%
    print_shape() %>%
    process_wave_10_presvote()
  }



# ----- RUN PROCESSING FUNCTIONS -----


zip_to_county <- clean_zips(zip_to_county_raw)
cdf1 <- clean_wave_6(df1)
cdf2 <- clean_wave_10(df2)

# ----- CREATE NEW VARIABLES -----


# # CROSS_PARTY_OPENNESS
# # Define "openness to vote cross-party" based on responses to
# # whether Democrats would 1) consider voting for Trump, or 2) consider voting
# # for any Republican at all over Biden. Similar for Republicans voting for Biden
# # or voting for any Democrat at all over Trump.
# openness <- df %>% mutate(
#     consider_other_candidate = ifelse(
#       collapsed_pid == "Independent/Not sure",
#       NA,
#       ifelse(
#         collapsed_pid == "Democrat",
#         consider_trump,
#         consider_biden
#       )
#     ),
#     consider_other_party_candidate = ifelse(
#       collapsed_pid == "Independent/Not sure",
#       NA,
#       ifelse(
#         collapsed_pid == "Democrat",
#         consider_any_republican,
#         consider_any_democrat
#       )
#     )
#   ) %>%
#       mutate(
#         openness = case_when(
#           is.na(consider_other_candidate) | is.na(consider_other_party_candidate) ~ NA,
#           as.numeric(consider_other_candidate) == 1 ~ "Very open",
#           consider_other_party_candidate == 1 ~ "Somewhat open",
#           .default="Not open"
#         )
#       ) %>%
#       pull(openness)
# 
# df$cross_party_openness <- factor(
#   openness,
#   levels=c("Not open", "Somewhat open", "Very open"),
#   ordered=TRUE
#   )



# ---- SAVE DF ----


saveRDS(processed_df1, 'dat/wave6/processed_wave_6.rds')
saveRDS(processed_df2, 'dat/wave10/processed_wave_10.rds')

