library(tidyr)
library(dplyr)
library(forcats)
library(haven)
library(ggplot2)
library(stringr)

setwd("C:/Users/Jared/Desktop/Columbia 2024 (3rd)/Gelman Lab/social-ties")

# ----- LOAD DATA -----

waves <- c(6,10)
raw_data_filenames <- c()
for (wave in waves) {
  raw_data_filenames <- c(raw_data_filenames, paste0('dat/wave', wave, '/raw_wave_', wave, '.rds'))
}
dfs <- lapply(raw_data_filenames, readRDS)

# df %>% count(pid7)
# df2 %>% count(pid7)
# ggplot(df, aes(pid7)) + geom_bar()
# ggplot() + 
#   geom_bar(data=df, aes(age4), fill='lightblue', alpha=.5) + 
#   geom_bar(data=df2, aes(age4), fill='blue', alpha=.2) + 
#   theme_minimal()

# ----- CLEAN/PROCESS VARIABLES -----

print_shape <- function(df) {
  cat("Rows:", nrow(df), "| Columns:", ncol(df), "\n")
  df
}

# Create mappings for Democrats and Republicans
copartisanship_levels <- c(
  "All co-partisan",
  "Mostly co-partisan",
  "About equal",
  "Mostly counter-partisan",
  "All counter-partisan",
  "Not sure"
)
friend_group_partisan_levels <- head(levels(df$friend_group_pid5), -1)
dem_levels <- c(friend_group_partisan_levels, "Not sure")
rep_levels <- c(rev(friend_group_partisan_levels), "Not sure")
dem_map <- setNames(copartisanship_levels, dem_levels)
rep_map <- setNames(copartisanship_levels, rep_levels)

clean <- function(df) {
  df %>%
    rename(urbanicity = urbancity) %>% # Rename column urbancity to urbanicity
    mutate(pid3 = factor(if_else(pid3 == "Other", "Independent", pid3)) %>%  # PARTY ID (pid3): Collapse "Other" to "Independent"
             fct_relevel("Democrat", "Independent", "Republican")) %>%
    # IMPUTE NA URBANICITY RESPONSE
    # one person didn't respond re urbanicity, but they're in a zip code with an urban population
    # 37,974 and rural population: 6,230 (from https://www.city-data.com/zips/46360.html)
    replace_na(list(urbanicity = "City")) %>% 
    # CHANGE "Other" RESPONSES IN URBANICITY TO NA
    # allows plotting functions to ignore the small number of (~8) "Other" responses
    mutate(urbanicity = if_else(urbanicity == "Other", NA, urbanicity)) %>%
    print_shape() %>%
    filter(pid7 != "Not sure") %>% # throw out people who responded "Not sure" to `pid7`
    print_shape() %>%
    drop_na(friend_group_pid3) %>%
    print_shape() %>%
    # FRIEND_GROUP_PID5
    # This variable separates out those who don't have any friends of the opposing
    # party from those who do. Essentially we break up the "Mostly D" and "Mostly R"
    # categories based on respondents' answers to questions about whether they have
    # any friends of the opposing party.
    # among those who are friends with "Mostly Democrats", change the covariate
    # value to "All Democrats" if `any_friends_republicans` is false.
    mutate(friend_group_pid5 = case_when(replace_na(friend_group_pid3 == "Mostly Democrats" & !(any_friends_republicans == "Yes"), FALSE) ~ "All Democrats",
                                         replace_na(friend_group_pid3 == "Mostly Republicans" & !(any_friends_democrats == "Yes"), FALSE) ~ "All Republicans",
                                         TRUE ~ friend_group_pid3) %>% 
             factor(levels = c("All Democrats","Mostly Democrats","About evenly split","Mostly Republicans","All Republicans","Not sure")),
           # FRIEND_GROUP_PID_NUMERIC
           # -2, -1, 0, 1, 2
           # For "All Democrats", ..., "All Republicans". Where we collapse "Not sure"
           # respondents to "About evenly split".
           friend_group_pid_numeric = if_else(friend_group_pid5 == "Not sure", 
                                              0, 
                                              as.numeric(friend_group_pid5) - 3),
           collapsed_pid = case_when(
             as.numeric(pid7) < 4 ~ "Democrat",
             as.numeric(pid7) > 4 & pid7 != "Not sure" ~ "Republican",
             TRUE ~ "Independent/Not sure"
             ) %>% fct_relevel("Democrat", "Independent/Not sure", "Republican"),
           friend_group_copartisanship = if_else(collapsed_pid == "Independent/Not sure", 
                                                 NA, 
                                                 if_else(collapsed_pid == "Democrat",
                                                         dem_map[as.character(friend_group_pid5)],
                                                         rep_map[as.character(friend_group_pid5)])) %>% 
             fct_relevel(copartisanship_levels),
           friend_group_copartisanship_numeric = if_else(friend_group_copartisanship == "Not sure", 
                                                         0, 
                                                         as.numeric(friend_group_copartisanship) - 3))
}

clean_wave_6 <- function(df) {
  df %>%
    mutate(friend_group_class = friend_group_class %>% 
             fct_recode("Friends much wealthier" = "Much wealthier",
                        "Friends slightly wealthier" = "Slightly wealthier",
                        "Friends the same" = "The same",
                        "Friends slightly poorer" = "Slightly poorer",
                        "Friends much poorer" = "Much poorer"))
}

clean(dfs[[1]])

processed_dfs <- lapply(dfs, clean)
processed_df1 <- clean_wave_6(processed_dfs[[1]])
processed_df2 <- processed_dfs[[2]]

processed_df2 %>% dplyr::select(contains('friend'))

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

# COUNTY_LEANING
zip_to_county <- read.csv('dat/geographic/zip_to_county.csv')

zip_to_char <- function(zip) {
  zip_char <- as.character(zip)
  ifelse(
    nchar(zip_char) == 5,
    zip_char,
    str_pad(zip_char, 5, pad = "0", side = "left")
  )
}

processed_df1 <- processed_df1 %>%
  mutate(
    #zipcode = zip_to_char(zipcode),
    state_lower = tolower(inputstate)
  ) %>%
  left_join(zip_to_county %>% dplyr::select(zipcode, state_lower, county), by=c('state_lower'='state_lower', 'zipcode'='zipcode'))

# ---- SAVE DF ----

saveRDS(processed_df1, 'dat/wave6/processed_wave_6.rds')
saveRDS(processed_df2, 'dat/wave10/processed_wave_10.rds')

# ---- OTHER DATA SOURCES ----

# STATE-LEVEL LEANINGS

# Data source: The Cook Political Report
# URL: https://cookpolitical.com/ratings/presidential-race-ratings
# Note: Data manually transcribed on September 10, 2024

cook_ratings <- tribble(
  ~category, ~state, ~votes,
  "Solid D", "California", 54,
  "Solid D", "Colorado", 10,
  "Solid D", "Connecticut", 7,
  "Solid D", "Delaware", 3,
  "Solid D", "Dist. of Columbia", 3,
  "Solid D", "Hawaii", 4,
  "Solid D", "Illinois", 19,
  "Solid D", "Maine(01)", 1,
  "Solid D", "Maryland", 10,
  "Solid D", "Massachusetts", 11,
  "Solid D", "New Jersey", 14,
  "Solid D", "New York", 28,
  "Solid D", "Oregon", 8,
  "Solid D", "Rhode Island", 4,
  "Solid D", "Vermont", 3,
  "Solid D", "Washington", 12,
  "Likely D", "Maine", 2,
  "Likely D", "Minnesota", 10,
  "Likely D", "New Hampshire", 4,
  "Likely D", "New Mexico", 5,
  "Likely D", "Virginia", 13,
  "Lean D", "Nebraska(02)", 1,
  "Toss Up", "Arizona", 11,
  "Toss Up", "Georgia", 16,
  "Toss Up", "Michigan", 15,
  "Toss Up", "Nevada", 6,
  "Toss Up", "North Carolina", 16,
  "Toss Up", "Pennsylvania", 19,
  "Toss Up", "Wisconsin", 10,
  "Likely R", "Florida", 30,
  "Likely R", "Maine(02)", 1,
  "Likely R", "Texas", 40,
  "Solid R", "Alabama", 9,
  "Solid R", "Alaska", 3,
  "Solid R", "Arkansas", 6,
  "Solid R", "Idaho", 4,
  "Solid R", "Indiana", 11,
  "Solid R", "Iowa", 6,
  "Solid R", "Kansas", 6,
  "Solid R", "Kentucky", 8,
  "Solid R", "Louisiana", 8,
  "Solid R", "Mississippi", 6,
  "Solid R", "Missouri", 10,
  "Solid R", "Montana", 4,
  "Solid R", "Nebraska", 2,
  "Solid R", "Nebraska(01)", 1,
  "Solid R", "Nebraska(03)", 1,
  "Solid R", "North Dakota", 3,
  "Solid R", "Ohio", 17,
  "Solid R", "Oklahoma", 7,
  "Solid R", "South Carolina", 9,
  "Solid R", "South Dakota", 3,
  "Solid R", "Tennessee", 11,
  "Solid R", "Utah", 6,
  "Solid R", "West Virginia", 4,
  "Solid R", "Wyoming", 3
)

head(cook_ratings)
write.csv(cook_ratings, "dat/cook_political_report_ratings.csv", row.names=FALSE)
