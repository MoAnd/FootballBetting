
# Housekeeping ------------------------------------------------------------

rm(list = ls())  # Clear all
graphics.off()   # Close all
cat("\014")      # Clears console

# Libraries ---------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, Rsolnp, lubridate, ggplot2, plotly, dygraphs, purrr)

source("football_likelihood_func.R")

# Data --------------------------------------------------------------------

load(file = "MatchBetData/EnglishMatchData.Rdata")

teams <- Match_data$HomeTeam %>% unique() %>% sort()

# matches <- list() # Not in use, but provides a nice overview
# for(i in teams){
#   matches[[i]] <- full_join(Match_data %>%
#                               filter(HomeTeam == i), Match_data %>%
#                               filter(AwayTeam == i)) %>%
#     arrange(Date)
# }


# Test --------------------------------------------------------------------

alpha <- rep(c(0.85, 1.15), length(teams)/2)
beta <- rnorm(length(teams), mean = 1, sd = 0.2)
rho <- 0.01
gam <- 1.4
theta <- c(alpha, beta, rho, gam)

# football_lik(theta, df = Match_data[1:1000,], teams = teams)

ind <- which(Match_data$Time <= 393)

result_dyna <- solnp(pars = theta, fun = football_lik, eqfun = equal,
                     eqB = length(teams), LB = c(rep(0, (length(theta)-1)), 1),
                     UB = rep(100, length(theta)),
                     df = Match_data[ind,], teams = teams, xi = 0.0065)

teamdf <- tibble(teams, 
                 result_dyna$pars[1:length(teams)], 
                 result_dyna$pars[(length(teams)+1):(2*length(teams))])

colnames(teamdf) = c("Team", "Alpha", "Beta")

# Optimizing the likelihood function --------------------------------------

time_start <- 106   # Chose your starting point

time_end <- Match_data$Time[nrow(Match_data)] %>% ceiling()

for (i in time_start:time_end) {
  
  alpha <- rep(c(0.85, 1.15), length(teams)/2)
  beta <- rnorm(length(teams), mean = 1, sd = 0.2)
  rho <- 0.01
  gam <- 1.4
  
  theta <- c(alpha, beta, rho, gam)
  
  ind <- which(Match_data$Time <= i)
  
  result_dyna <- solnp(pars = theta, fun = football_lik, eqfun = equal,
                       eqB = length(teams), LB = c(rep(0, (length(theta)-1)), 1),
                       UB = rep(100, length(theta)),
                       df = Match_data[ind,], teams = teams)
  
  teamdf <- tibble(teams, result_dyna$pars[1:length(teams)], result_dyna$pars[(length(teams)+1):(2*length(teams))])
  colnames(teamdf) = c("Team", "Alpha", "Beta")
  
  write_csv(teamdf, path = paste0("ParameterData/Teamdfs/teamdf-", i, ".csv"))
  write_csv(tibble("Rho" = result_dyna$pars[(length(theta) - 1)]), path = "ParameterData/rho.csv", append = TRUE)
  write_csv(tibble("HomeAdvantage" = result_dyna$pars[length(theta)]), path = "ParameterData/HomeAdvantage.csv", append = TRUE)
  
}

# Team's alpha/beta through time ------------------------------------------

Alpha_beta_df <- tibble(Time = rep(time_start, length(teams))) %>% 
  bind_cols(read_csv(paste0("ParameterData/Teamdfs/teamdf-", time_start, ".csv"))) %>% 
  gather("Type", "Value", 3:4)

for (i in (time_start+1):time_end) {
  Alpha_beta_df <- Alpha_beta_df %>%
    bind_rows(tibble(Time = rep(i, length(teams))) %>%
                bind_cols(read_csv(paste0("ParameterData/Teamdfs/teamdf-",i,".csv"))) %>%
                gather("Type", "Value", 3:4))
}

save(Alpha_beta_df, file = "ParameterData/Alpha_beta_df.Rdata")

# Rho/gamma ---------------------------------------------------------------

Rho_lambda_df <- tibble("Time" = time_start:time_end) %>%        # I know the name here is lambda, and I don't really care for now....
  bind_cols(read_csv("ParameterData/HomeAdvantage.csv")) %>%
  bind_cols(read_csv("ParameterData/rho.csv")) %>%
  gather("Parameter", "Value", 2:3)

save(Rho_lambda_df, file = "ParameterData/Rho_Homeadvantage_df.Rdata")


