library(dplyr)
library(forcats)

RAW_DATA_FILENAME <- 'dat/RAW.rds'

# LOAD DATA
df <- readRDS(RAW_DATA_FILENAME)

# ----- CLEAN/PROCESS VARIABLES -----

# PARTY ID (pid3)
# collapse "Other" to "Independent"
df$pid3[df$pid3 == "Other"] <- "Independent"
# throw out people who responded "Not sure" to `pid7`
df <- df[df$pid7 != "Not sure",]
# relevel the factor
df$pid3 <- factor(df$pid3)
df$pid3 <- fct_relevel(df$pid3, "Democrat", "Independent", "Republican")

# REMOVE MISSING FRIEND_GROUP_PID3
# (there's only 8 people who didn't answer)
df <- df[!is.na(df$friend_group_pid3),]

# RENAME FRIEND_GROUP_CLASS FACTOR LEVELS
df$friend_group_class <- fct_recode(df$friend_group_class,
  "Friends much wealthier" = "Much wealthier",
  "Friends slightly wealthier" = "Slightly wealthier",
  "Friends the same" = "The same",
  "Friends slightly poorer" = "Slightly poorer",
  "Friends much poorer" = "Much poorer"
)

# RENAME "urbancity" TO "urbanicity"
df <- df %>% rename(urbanicity=urbancity)

# IMPUTE NA URBANICITY RESPONSE
# there's one person who didn't respond. they're in a zip code
# with urban population: 37,974 and rural population: 6,230
# (from https://www.city-data.com/zips/46360.html)
# so just mark them as urban
df$urbanicity[is.na(df$urbanicity)] <- "City"

# CHANGE "Other" RESPONSES TO URBANICITY TO NA
# this way we don't throw out the rows outright but
# our plotting functions know to ignore them since there are so few.
df[df$urbanicity == "Other" & !is.na(df$urbanicity), "urbanicity"] <- NA

# ----- CREATE NEW VARIABLES -----

# FRIEND_GROUP_PID5
# This variable separates out those who don't have any friends of the opposing
# party from those who do. Essentially we break up the "Mostly D" and "Mostly R"
# categories based on respondents' answers to questions about whether they have
# any friends of the opposing party.
df$friend_group_pid5 <- df$friend_group_pid3
df$friend_group_pid5 <- factor(df$friend_group_pid5, levels=c(
  "All Democrats",
  "Mostly Democrats",
  "About evenly split",
  "Mostly Republicans",
  "All Republicans",
  "Not sure"
))
# among those who are friends with "Mostly Democrats", change the covariate
# value to "All Democrats" if `any_friends_republicans` is false.
all_d <- df$friend_group_pid5 == "Mostly Democrats" & !(df$any_friends_republicans == "Yes")
all_d[is.na(all_d)] <- FALSE
df$friend_group_pid5[all_d] <- "All Democrats"
all_r <- df$friend_group_pid5 == "Mostly Republicans" & !(df$any_friends_democrats == "Yes")
all_r[is.na(all_r)] <- FALSE
df$friend_group_pid5[all_r] <- "All Republicans"

# FRIEND_GROUP_PID_NUMERIC
# -2, -1, 0, 1, 2
# For "All Democrats", ..., "All Republicans". Where we collapse "Not sure"
# respondents to "About evenly split".
df <- df %>% mutate(
  friend_group_pid_numeric = ifelse(friend_group_pid5 == "Not sure", 0, as.numeric(friend_group_pid5) - 3),
)

# COLLAPSED_PID
# Collapse people who "lean Dem" or "lean Rep" into their respective parties
df$collapsed_pid <- "Independent/Not sure"
lean_dem <- as.numeric(df$pid7) < 4
lean_rep <- as.numeric(df$pid7) > 4 & (df$pid7 != "Not sure")
df$collapsed_pid[lean_dem] <- "Democrat"
df$collapsed_pid[lean_rep] <- "Republican"

df$collapsed_pid <- factor(df$collapsed_pid, levels=c(
  "Democrat",
  "Independent/Not sure",
  "Republican"
))

# FRIEND_GROUP_COPARTISANSHIP
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
# Create mappings for Democrats and Republicans
dem_map <- setNames(copartisanship_levels, dem_levels)
rep_map <- setNames(copartisanship_levels, rep_levels)

# Create the new variable
df$friend_group_copartisanship <- ifelse(
  df$collapsed_pid == "Independent/Not sure", NA,
       ifelse(df$collapsed_pid == "Democrat",
              dem_map[as.character(df$friend_group_pid5)],
              rep_map[as.character(df$friend_group_pid5)])
)

# Convert the new variable to a factor with the specified levels
df$friend_group_copartisanship <- factor(df$friend_group_copartisanship, 
                                         levels = copartisanship_levels)

# FRIEND_GROUP_COPARTISANSHIP_NUMERIC
# -2, -1, 0, 1, 2
# For "All Copartisan", ..., "All Counterpartisan". Where we collapse "Not sure"
# respondents to "About evenly split".
df <- df %>% mutate(
  friend_group_copartisanship_numeric = ifelse(
    friend_group_copartisanship == "Not sure", 0, as.numeric(friend_group_copartisanship) - 3
    ),
)

# CROSS_PARTY_OPENNESS
# Define "openness to vote cross-party" based on responses to
# whether Democrats would 1) consider voting for Trump, or 2) consider voting
# for any Republican at all over Biden. Similar for Republicans voting for Biden
# or voting for any Democrat at all over Trump.
openness <- df %>% mutate(
    consider_other_candidate = ifelse(
      collapsed_pid == "Independent/Not sure",
      NA,
      ifelse(
        collapsed_pid == "Democrat",
        consider_trump,
        consider_biden
      )
    ),
    consider_other_party_candidate = ifelse(
      collapsed_pid == "Independent/Not sure",
      NA,
      ifelse(
        collapsed_pid == "Democrat",
        consider_any_republican,
        consider_any_democrat
      )
    )
  ) %>%
      mutate(
        openness = case_when(
          is.na(consider_other_candidate) | is.na(consider_other_party_candidate) ~ NA,
          as.numeric(consider_other_candidate) == 1 ~ "Very open",
          consider_other_party_candidate == 1 ~ "Somewhat open",
          .default="Not open"
        )
      ) %>%
      pull(openness)

df$cross_party_openness <- factor(
  openness,
  levels=c("Not open", "Somewhat open", "Very open"),
  ordered=TRUE
  )

# COUNTY_LEANING
zip_to_county_raw <- read.csv('dat/zip_to_county.csv')

zip_to_char <- function(zip) {
  zip_char <- as.character(zip)
  ifelse(
    nchar(zip_char) == 5,
    zip_char,
    str_pad(zip_char, 5, pad = "0", side = "left")
  )
}

df <- df %>%
  mutate(
    zipcode = zip_to_char(zipcode),
    state_lower = tolower(inputstate)
    ) %>%
  left_join(zip_to_county %>% dplyr::select(zipcode, state_lower, county), by=c('state_lower'='state_lower', 'zipcode'='zipcode'))


# ---- SAVE DF ----
processed <- df
saveRDS(processed, 'dat/processed.rds')

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
