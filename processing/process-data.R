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

# df %>% count(pid7)
# df2 %>% count(pid7)
# ggplot(df, aes(pid7)) + geom_bar()
# ggplot() + 
#   geom_bar(data=df, aes(age4), fill='lightblue', alpha=.5) + 
#   geom_bar(data=df2, aes(age4), fill='blue', alpha=.2) + 
#   theme_minimal()


# ----- CLEAN/PROCESS VARIABLES -----


# Utility Functions -----

print_shape <- function(df) {
  cat("Rows:", nrow(df), "| Columns:", ncol(df), "\n")
  df}

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


# Copartisanship Mappings -----

copartisanship_levels <- c(
  "All co-partisan",
  "Mostly co-partisan",
  "About equal",
  "Mostly counter-partisan",
  "All counter-partisan",
  "Not sure")

friend_group_partisan_levels <- c(
  "All Democrats",
  "Mostly Democrats",
  "About evenly split",
  "Mostly Republicans",
  "All Republicans")

dem_levels <- c(friend_group_partisan_levels, "Not sure")
rep_levels <- c(rev(friend_group_partisan_levels), "Not sure")

dem_map <- setNames(copartisanship_levels, dem_levels)
rep_map <- setNames(copartisanship_levels, rep_levels)

# Survey Cleaning Function -----

clean_survey <- function(df) {
  
  df %>%
    rename(urbanicity = urbancity) %>% # Rename column urbancity to urbanicity
    
    # There is 1 NA in urbancity with an urban zipcode: population 37,974 urban vs 6,230 rural (https://www.city-data.com/zips/46360.html)
    replace_na(list(urbanicity = "City")) %>% 
    
    mutate(
      pid3 = recode(pid3, "Other" = "Independent") %>%  fct_relevel(
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
      urbanicity = recode(urbanicity, "Other" = NA)  # Chang "Other" to NA for plotting functions to ignore the ~8 "Other" responses
    ) %>%
    
    print_shape() %>%
    
    # filter(pid7 != "Not sure") %>% # throw out people who responded "Not sure" to `pid7`

    drop_na(friend_group_pid3) %>%
    
    print_shape() %>%
    
    # Do participants have any friends of the opposite party?
    mutate(
      friend_group_pid5 = case_when(
        replace_na(friend_group_pid3 == "Mostly Democrats" & !(any_friends_republicans == "Yes"), FALSE) ~ "All Democrats",
        replace_na(friend_group_pid3 == "Mostly Republicans" & !(any_friends_democrats == "Yes"), FALSE) ~ "All Republicans",
        TRUE ~ friend_group_pid3
      ) %>% fct_relevel(
        "All Democrats",
        "Mostly Democrats",
        "About evenly split",
        "Mostly Republicans",
        "All Republicans",
        "Not sure"),
      
      friend_group_pid_numeric = if_else(
        friend_group_pid5 == "Not sure", 
        0, 
        (as.numeric(friend_group_pid5) - 3))/2,
      
      collapsed_pid = case_when(
        as.numeric(pid7) < 4 ~ "Democrat",
        as.numeric(pid7) > 4 ~ "Republican",
        TRUE ~ "Independent"
      ) %>% fct_relevel(
        "Democrat", 
        "Independent", 
        "Republican"),
      
      collapsed_pid_numeric = if_else(collapsed_pid == "Independent/Not sure", 
                                      0, 
                                      as.numeric(collapsed_pid) - 2),
      
      collapsed_pid_baseline = case_when(
        as.numeric(pid7_baseline) < 4 ~ "Democrat",
        as.numeric(pid7_baseline) > 4 & pid7 != "Not sure" ~ "Republican",
        TRUE ~ "Independent/Not sure"
       ) %>% fct_relevel(
         "Democrat", 
         "Independent/Not sure", 
         "Republican"),
      
      collapsed_pid_baseline_numeric = if_else(
        collapsed_pid_baseline == "Independent/Not sure",
        0, 
        as.numeric(collapsed_pid_baseline) - 2),
      
      friend_group_copartisanship = if_else(
        collapsed_pid == "Independent/Not sure", 
        NA, 
        if_else(
          collapsed_pid == "Democrat",
          dem_map[as.character(friend_group_pid5)],
          rep_map[as.character(friend_group_pid5)])
      ) %>% fct_relevel(copartisanship_levels),
      
      friend_group_copartisanship_numeric = if_else(
        friend_group_copartisanship == "Not sure", 
        0, 
        as.numeric(friend_group_copartisanship) - 3),
      
      zipcode = zip_to_char(zipcode),
      state_lower = tolower(inputstate)
      
    ) %>%
    
    left_join(zip_to_county %>% dplyr::select(zipcode, state_lower, county), 
              by = c('state_lower' = 'state_lower', 'zipcode' = 'zipcode'))
  
}

clean_wave_6 <- function(df) {
  df %>%
    print_shape() %>%
    
    drop_na(presvote24, friend_group_presvote24) %>%
    
    print_shape() %>%
    
    mutate(friend_group_class = friend_group_class %>% 
             fct_recode("Friends much wealthier" = "Much wealthier",
                        "Friends slightly wealthier" = "Slightly wealthier",
                        "Friends the same" = "The same",
                        "Friends slightly poorer" = "Slightly poorer",
                        "Friends much poorer" = "Much poorer"),
           
           presvote24 = case_when(
               presvote24 == "Joe Biden" ~ "Democrat",
               presvote24 == "Donald Trump" ~ "Republican",
               TRUE ~ "Other"
             ) %>% fct_relevel("Democrat", "Other", "Republican"),
           
           presvote24 = case_when(
               presvote24 == "Other" & consider_biden == "I would consider voting for Joe Biden" & consider_trump == "I would consider voting for Donald Trump" ~ "Other",
               presvote24 == "Other" & consider_biden == "I would consider voting for Joe Biden" ~ "Democrat",
               presvote24 == "Other" & consider_trump == "I would consider voting for Donald Trump" ~ "Republican",
               TRUE ~ presvote24
             ),
             
           friend_group_presvote24 = case_when(
               friend_group_presvote24 == "Mostly Joe Biden" ~ "Democrat",
               friend_group_presvote24 == "Mostly Donald Trump" ~ "Republican",
               TRUE ~ "About evenly split"
             ) %>% fct_relevel("Democrat", "About evenly split", "Republican"),

           best_friend_presvote24 = case_when(
               best_friend_presvote24 == "Joe Biden" ~ "Democrat",
               best_friend_presvote24 == "Donald Trump" ~ "Republican",
               TRUE ~ "Other"
             ) %>% fct_relevel("Democrat", "Other", "Republican"),
           
           presvote24_numeric = as.numeric(as.factor(presvote24)) - 2,
           friend_group_presvote24_numeric = as.numeric(as.factor(friend_group_presvote24)) - 2
          )
}

clean_wave_10 <- function(df) {
  df %>%
    print_shape() %>%
    
    drop_na(presvote24h, friend_group_presvote24h) %>% 
    
    print_shape() %>%
    
    mutate(
      presvote24h = case_when(
          presvote24h == "Kamala Harris" ~ "Democrat",
          presvote24h == "Donald Trump" ~ "Republican",
          TRUE ~ "Other"
        ) %>% fct_relevel("Democrat", "Other", "Republican"),
      
      presvote24h = case_when(
          presvote24h == "Other" & consider_harris == "I would consider voting for Kamala Harris" & consider_trump == "I would consider voting for Donald Trump" ~ "Other",
          presvote24h == "Other" & consider_harris == "I would consider voting for Kamala Harris" ~ "Democrat",
          presvote24h == "Other" & consider_trump == "I would consider voting for Donald Trump" ~ "Republican",
          TRUE ~ presvote24h
        ) %>% fct_relevel("Democrat", "Other", "Republican"),

      friend_group_presvote24h = case_when(
          friend_group_presvote24h == "Mostly Kamala Harris" ~ "Democrat",
          friend_group_presvote24h == "Mostly Donald Trump" ~ "Republican",
          TRUE ~ "About evenly split"
        ) %>% fct_relevel("Democrat", "About evenly split", "Republican"),

      presvote24post = case_when(
          presvote24post == "Kamala Harris" ~ "Democrat",
          presvote24post == "Donald Trump" ~ "Republican",
          TRUE ~ "Other"
        ) %>% fct_relevel("Democrat", "Other", "Republican"),
      
      presvote24h_numeric = as.numeric(as.factor(presvote24h)) - 2,
      friend_group_presvote24h_numeric = as.numeric(as.factor(friend_group_presvote24h)) - 2)
  
}


zip_to_county <- clean_zips(zip_to_county_raw)
processed_dfs <- lapply(dfs, clean_survey)
processed_df1 <- clean_wave_6(processed_dfs[[1]])
processed_df2 <- clean_wave_10(processed_dfs[[2]])

# processed_df1$presvote24_numeric
# processed_df2$presvote24h_numeric

# 
# further_collapsed_pid = if_else(
#   collapsed_pid == "Independent/Not sure" and consider_harris == "I would consider voting for Kamala",
#   NA,
#   if_else(
#     collapsed_pid == "Democrat",
#     consider_trump,
#     consider_biden
#   )
# )

# # ----- CREATE NEW VARIABLES -----
# 
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

