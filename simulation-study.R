library(dplyr)
library(ggplot2)
library(MASS)

logit <- function(x) {
  return(log(x/(1-x)))
}
invlogit <- function(x) {
  e_x <- exp(x)
  return(e_x / (1 + e_x))
}

simulate_data <- function(N) {
  # P_i, party_id. coded as -1 for D, 1 for R.
  party_id <- (2 * rbinom(N, 1, 0.5)) - 1
  
  # Let's say that people who are Republican answer according to the
  # following distribution:
  #   All D (-2):        0.03
  #   Mostly D (-1):     0.07
  #   Roughly even (0):  0.1
  #   Mostly R (1):      0.5
  #   All R (2):         0.3
  # and assume the exact reverse for Democrats
  
  # L_i, local social partisanship
  ls_probs <- c(0.03, 0.07, 0.1, 0.5, 0.3)
  local_social <- sample(seq(-2, 2), N, replace=TRUE, prob=ls_probs)
  
  # multiply by -1 if Democrat to flip the probabilities
  local_social <- local_social * party_id
  
  # For simplicity, start with one economic question on a 3-point scale.
  # For example: "The current income tax rate for the highest tax bracket is
  # 37%. Should it be raised, lowered or kept the same?"
  #     1) Raised 2) Kept the same 3) Lowered
  # Denote this by Y_i.
  
  # The ordinal regression model for this problem can be written as:
  # P(Y_i > -1) = invlogit(a + XB)
  # P(Y_i > 0) = invlogit(a + XB - c)
  
  # Additionally, we treat the categorical predictor L_i as a continuous
  # variable, essentially meaning that we assume the "effect" on policy
  # preferences is constant when comparing -1 to 0 or 0 to 1. This way,
  # we only have one parameter whose significance we need to assess, simplifying
  # the power analysis.
  
  # Our model is thus:
  #    P(Y_i > -1) = invlogit(a0 * 1(P_i = -1) + a1 * 1(P_i = 1) + b * L_i),
  #    P(Y_i > 0) = invlogit(a0 * 1(P_i = -1) + a1 * 1(P_i = 1) + b * L_i - c),
  # We don't include an intercept term so we can have a0 and a1 individually.
  
  # Define our true parameter values
  
  # Coefficients for party ID in the regression
  a0 <- -2
  a1 <- 4
  
  # Coefficient for local social in the regression
  b <- 0.5
  
  # Additional person-level variation not explained by party ID or local social
  sigma <- 1
  eps <- rnorm(N, 0, sigma)
  
  # Cutoff parameter shifting log-odds for P(Y_i > 0)
  c <- 2
  
  # Calculate the base log-odds, logit[ P(Y_i > -1) ]
  log_odds <- (ifelse(party_id == -1, a0, a1) + b * local_social + eps)
  
  # Calculate the actual probabilities of each outcome
  p1 <- invlogit(log_odds) # P(Y_i > -1)
  p2 <- invlogit(log_odds - c) # P(Y_i > 0)
  
  # We obtain the probabilities for specific outcome values as follows
  #   P(Y_i = -1) = 1 - P(Y_i > -1) = 1 - p1
  #   P(Y_i = 0) = P(Y_i > -1) - P(Y_i > 0) = p1 - p2
  #   P(Y_i = 1) = P(Y_i > 0) = p2
  p <- cbind(1 - p1, p1 - p2, p2)
  colnames(p) <- c("p1", "p2", "p3")
  
  # Generate outcome variable
  y <- rep(0, N)
  for (i in 1:N) {
    y[i] <- sample(-1:1, 1, prob=p[i,])
  }
  
  result <- data.frame(cbind(party_id, local_social, y, p))
  
  # Create some nice helper columns
  result$pid <- ifelse(result$party_id == -1, "Democrat", "Republican")
  result$pid <- factor(result$pid, levels=c("Democrat", "Republican"))
  local_social_levels <- c("All D", "Mostly D", "Even", "Mostly R", "All R")
  result$ls <- factor(
    local_social_levels[result$local_social + 3],
    levels=local_social_levels
    )
  
  # change `y` to a factor for use in polr down the line
  result$y <- factor(result$y, levels=-1:1)
  
  return(result)
}

extract_coefs_categorical <- function (fit) {
  coefs <- fit$coefficients
  
  # NOTE: `polr` models logit P(Y_i > 0) as d - XB instead of XB - d. We
  # follow the latter, using notation from (Gelman and Hill, 2021). As such,
  # we need to negate most coefficients.
  
  # party_id coefficients (reusing the notation from `simulate`)
  a0 <- -fit$zeta["-1|0"]
  a1 <- -coefs["pidRepublican"]
  
  # local social coefficients
  prefix <- "lsR"
  b02 <- coefs[paste0(prefix, "All D")]
  b01 <- coefs[paste0(prefix, "Mostly D")]
  b11 <- coefs[paste0(prefix, "Mostly R")]
  b12 <- coefs[paste0(prefix, "All R")]
  
  # cutoff term for second response, P(Y_i > 0)
  d <- fit$zeta["0|1"] - fit$zeta["-1|0"]
  
  coefs <- list(a0=a0, a1=a1, b02=b02, b01=b01, b11=b11, b12=b12, d=d)
  
  # convert to numeric to get rid of any labeling from the output of `fit`
  for (i in 1:length(coefs)) {
    coefs[[i]] <- as.numeric(coefs[[i]])
  }
  
  return(coefs)
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
  model <- polr(y ~ pid + local_social, data, Hess=TRUE)
  return(model)
}

run_sims <- function (n_iter, n_sample, alpha=0.05) {
  ests <- rep(0, n_iter)
  ses <- rep(0, n_iter)
  signif <- rep(FALSE, n_iter)
  
  for (i in 1:n_iter) {
    data <- simulate_data(n_sample)
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

check_power <- function(n_iter, n_sample, alpha=0.05) {
  result <- run_sims(n_iter=N_iter, n_sample=N)
  return(mean(result$signif))
}

data <- simulate_data(10000)
plot_simulation(data)
display(fit_model(data))

N_iter <- 1000
N <- 400
print(paste0("Power, N = ", N, ": ", check_power(N_iter, N)))