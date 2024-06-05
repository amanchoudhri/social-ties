library(dplyr)
library(ggplot2)
library(MASS)
library(patchwork)
library(arm)

# Helper function to create model parameters intuitively
make_params <- function(p_dem, p_rep, p_counter_dem, c, sigma=0) {
  # `p_dem`: desired P(Y_i = 0 | P_i = 0, L_i = 0), which is the probability that a
  # Democrat (P_i = 0) with mostly Democratic friends (L_i = 0) has a co-partisan
  # policy preference (Y_i = 0).
  
  # `p_rep`: desired P(Y_i = 0 | P_i = 1, L_i = 0), which is the probability that a
  # Republican (P_i = 1) with mostly Republican friends (L_i = 0) has a co-partisan
  # policy preference (Y_i = 0).
  
  # `p_counter_dem`: desired P(Y_i = 2 | P_i = 0, L_i = 0), which is the probability that a
  # Democrat (P_i = 0) with mostly Democratic friends (L_i = 0) takes a completely
  # counter-partisan policy preference (Y_i = 2).
  
  # `c`: coefficient for counterpartisanship in social group. The higher this
  # is, the greater the correlation between counterpartisan friends and
  # counterpartisan policy preferences.
  
  # `sigma`: scale for logistic distribution representing additional
  # person-level variation not explained by party ID or local social
  
  # Intercept
  a <- logit(1 - p_dem)
  
  # Coefficient for Republican
  b <- logit(1 - p_rep) - a
  
  # Cutoff parameter shifting log-odds for P(Y_i > 1)
  d <- a - logit(p_counter_dem)
  
  params <- list(a=a, b=b, c=c, d=d, sigma=sigma)
  return(params)
}

# Helper function to calculate policy preference probabilities for each person
probs <- function(party_id, local_social, params, noise=0) {
  # `party_id`: vector with entries in {0, 1}
  # `local_social`: vector with entries in {0, 1, 2}
  # `params`: list, output from `make_params`
  # `noise`: optional, vector of random individual-level logistic noise
  
  # Calculate the base log-odds, logit[ P(Y_i > 0) ]
  log_odds <- params$a + params$b * party_id + params$c * local_social + noise
  
  # Calculate the actual probabilities of each policy preference
  p_gt_0 <- invlogit(log_odds) # P(Y_i > 0)
  p_gt_1 <- invlogit(log_odds - params$d) # P(Y_i > 1)
  
  # We obtain the probabilities for specific preference values as follows
  #   P(Y_i = 0) = 1 - P(Y_i > 0) = 1 - p_gt_0
  #   P(Y_i = 1) = P(Y_i > 0) - P(Y_i > 1) = p_gt_0 - p_gt_1
  #   P(Y_i = 2) = P(Y_i > 1) = p_gt_1
  p <- cbind(1 - p_gt_0, p_gt_0 - p_gt_1, p_gt_1)
  colnames(p) <- c("p0", "p1", "p2")
  return(p)
}

add_helper_columns <- function(df) {
  # `df`: data.frame containing columns `party_id`, `local_social`
  party_names <- c("Democrat", "Republican")
  local_social_levels <- c("Mostly Co-Partisan", "Even", "Mostly Counter-Partisan")
  
  pid <- factor(party_names[df$party_id + 1], levels=party_names)
  
  ls <- factor(
    local_social_levels[df$local_social + 1],
    levels=local_social_levels
  )
  
  return(data.frame(cbind(pid, ls, df)))
}

simulate_data <- function(N, params) {
  # P_i, party_id. coded as 0 for D, 1 for R.
  party_id <- rbinom(N, 1, 0.5)
  
  # Let's say that people answer according to the following distribution:
  #   Mostly co-partisan (0):        0.75
  #   Roughly even (1):              0.2
  #   Mostly counter-partisan (2):   0.05
  
  # L_i, local social partisanship
  ls_probs <- c(0.75, 0.15, 0.1)
  local_social <- sample(0:2, N, replace=TRUE, prob=ls_probs)
  
  # For simplicity, start with one economic question on a 3-point scale.
  # For example: "The current income tax rate for the highest tax bracket is
  # 37%. Should it be raised, lowered or kept the same?"
  #     i) Raised ii) Kept the same iii) Lowered
  # Denote this by Y_i. Code the variable such that
  #     0: Expected partisan position. "Raised" for D, "Lowered" for R.
  #     1: Middle option.
  #     2: Counter-partisan position. "Lowered" for D, "Raised" for R.
  
  # The ordinal regression model for this problem can be written as:
  # P(Y_i > 0) = invlogit(a + XB)
  # P(Y_i > 1) = invlogit(a + XB - c)
  
  # Additionally, we treat the categorical predictor L_i as a continuous
  # variable, essentially meaning that we assume the "effect" on policy
  # preferences is the samewhen comparing 0 to 1 or 1 to 2. This way,
  # we only have one parameter whose significance we need to assess, simplifying
  # the power analysis.
  
  # Our model is thus:
  #    P(Y_i > 0) = invlogit(a + b * P_i + c * L_i + epsilon),
  #    P(Y_i > 1) = invlogit(a + b * P_i + c * L_i - d + epsilon).
  
  # Generate additional person-level variation not explained by party ID or local social
  eps <- rnorm(N, 0, params$sigma)
  
  # Calculate the probabilities of each person having each preference,
  # based on the proided params a, b, c, d, and the generated variation epsilon.
  p <- probs(party_id, local_social, params, eps)
  
  # Generate outcome variable from these probabilities
  y <- rep(0, N)
  for (i in 1:N) {
    y[i] <- sample(0:2, 1, prob=p[i,])
  }
  
  result <- data.frame(cbind(party_id, local_social, y, p))
  
  # Add some helper columns
  result <- add_helper_columns(result)
  
  # change `y` to a factor for use in polr down the line
  result$y <- factor(result$y, levels=0:2)
  
  return(result)
}

plot_simulation <- function(data) {
  N <- nrow(data)
  print(
    ggplot(data, aes(x=pid)) +
    geom_bar() +
    ggtitle(paste0("Party Breakdown, N = ", N))
    )
  
  print(
    ggplot(data, aes(x=ls)) +
    geom_bar() +
    ggtitle(paste0("Local Social Breakdown, N = ", N))
    )
  
  print(
    ggplot(data, aes(x=ls)) +
    geom_bar() +
    facet_grid(rows=vars(pid)) +
    ggtitle(paste0("Local Social Breakdown by Party ID, N = ", N))
    )
  
  # probs
  data %>% distinct(party_id, local_social, .keep_all=TRUE) %>%
    arrange(party_id, local_social) %>%
    dplyr::select(-c(y, party_id, local_social)) %>%
    relocate(pid, ls)
  
  print(
    ggplot(data, aes(x=y)) +
    geom_bar(aes(y=after_stat(prop), group=1)) +
    facet_grid(vars(pid)) +
    ggtitle(paste0("Policy Preference (Proportions) by Party ID, N = ", N))
    )
  
  print(
    ggplot(data, aes(x=y)) +
    geom_bar() +
    facet_grid(vars(pid), vars(ls)) +
    ggtitle(paste0("Policy Preference (Counts) by Party ID and Local Social, N = ", N))
    )
  
  print(
    ggplot(data, aes(x=y)) +
    geom_bar(aes(y=..prop.., group=1)) +
    facet_grid(vars(pid), vars(ls)) +
    ggtitle(paste0("Policy Preference (Proportions) by Party ID and Local Social, N = ", N))
    )
}
fit_model <- function (data) {
  model <- polr(y ~ party_id + local_social, data, Hess=TRUE)
  return(model)
}

run_sims <- function (n_iter, n_sample, params, alpha=0.05) {
  ests <- rep(0, n_iter)
  ses <- rep(0, n_iter)
  signif <- rep(FALSE, n_iter)
  
  for (i in 1:n_iter) {
    data <- simulate_data(n_sample, params)
    g <- fit_model(data)
    
    ests[i] <- as.numeric(g$coefficients["local_social"])
    ses[i] <- sqrt(diag(vcov(g))["local_social"])
    
    ci <- suppressMessages(confint(g, level=1-alpha))
    lower_gt_0 <- ci["local_social", 1] > 0
    upper_lt_0 <- ci["local_social", 2] < 0
    signif[i] <- lower_gt_0 || upper_lt_0
  }
  return(data.frame(cbind(ests, ses, signif)))
}

check_power <- function(n_iter, n_sample, params, alpha=0.05) {
  result <- run_sims(N_iter, N, params, alpha=alpha)
  return(mean(result$signif))
}

# p that a democrat with mostly dem friends has a dem view
p_dem <- 0.75 
# p that a republican with mostly rep friends has rep view
p_rep <- 0.8 
# p that a dem with mostly dem friends has a rep view
p_counter_dem <- 0.05

# "effect" size. higher c means it's more likely that people with
# counter-partisan friends will have counter-partisan views
c <- 0.8

# noise scale
sigma <- 1

params <- make_params(p_dem, p_rep, p_counter_dem, c, sigma)

data <- simulate_data(10000, params)
plot_simulation(data)
display(fit_model(data))

N_iter <- 1000
N <- 300
print(paste0("Power, N = ", N, ": ", check_power(N_iter, N, params)))
