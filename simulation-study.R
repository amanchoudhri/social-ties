library(dplyr)
library(ggplot2)

logit <- function(x) {
  return(log(x/(1-x)))
}
invlogit <- function(x) {
  e_x <- exp(x)
  return(e_x / (1 + e_x))
}

simulate <- function(N) {
  # P_i, party_id. coded as -1 for D, 1 for R.
  party_id <- (2 * rbinom(N, 1, 0.5)) - 1
  
  # let's say that people who are Republican answer local social
  # partisanship according to the following distribution
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
  
  # Converting categorical predictors as appropriate, our simple model is thus:
  # P(Y_i > -1) = invlogit(
  #   a0 * 1(P_i = -1) +
  #   a1 * 1(P_i = 1) +
  #   b02 * 1(L_i = -2) + b01 * 1(L_i = -1)
  #   b11 * 1(L_i = 1) + b12 * 1(L_i = 2)
  # ),
  # and P(Y_i > 0) similarly, with log-odds shifted by c. We don't include an
  # intercept term here so we can have a0 and a1 individually.
  
  # Define our true parameter values
  a0 <- -2
  a1 <- 2
  
  b02 <- -0.35
  b01 <- -0.2
  b11 <- 0.2
  b12 <- 0.35
  
  c <- 1
  
  log_odds <- (
    ifelse(party_id == -1, a0, a1) +
    ifelse(
      local_social == 0,
      0,
      ifelse(
        local_social < 0,
        ifelse(local_social == -1, b01, b02),
        ifelse(local_social == 1, b11, b12)
       )
    )
  )
  p1 <- invlogit(log_odds) # P(Y_i > -1)
  p2 <- invlogit (log_odds - c) # P(Y_i > 0) = P(Y_i = 1)
  
  p <- cbind(1 - p1, p1 - p2, p2)
  colnames(p) <- c("p1", "p2", "p3")
  
  y <- rep(0, N)
  for (i in 1:N) {
    y[i] <- sample(-1:1, 1, prob=p[i,])
  }
  result <- data.frame(cbind(party_id, local_social, y, p))
  
  # create some helper nice columns
  result$pid <- ifelse(result$party_id == -1, "Democrat", "Republican")
  result$pid <- factor(result$pid, levels=c("Democrat", "Republican"))
  local_social_levels <- c("All D", "Mostly D", "Even", "Mostly R", "All R")
  result$ls <- factor(local_social_levels[result$local_social + 3], levels=local_social_levels)
  
  return(result)
}

r <- simulate(1000)

ggplot(r, aes(x=pid)) + geom_bar()

ggplot(r, aes(x=ls)) + geom_bar()
ggplot(r, aes(x=ls)) + geom_bar() + facet_grid(rows=vars(pid))

# probs
r %>% distinct(party_id, local_social, .keep_all=TRUE) %>%
  arrange(party_id, local_social) %>%
  dplyr::select(-c(y, party_id, local_social)) %>%
  relocate(pid, ls)

ggplot(r, aes(x=y)) + geom_bar() + facet_grid(vars(pid), vars(ls))

r$y <- factor(r$y, levels=-1:1)

polr(y ~ pid + ls, r)